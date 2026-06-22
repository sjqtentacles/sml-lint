# sml-lint

[![CI](https://github.com/sjqtentacles/sml-lint/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-lint/actions/workflows/ci.yml)

A pure, deterministic **static linter for Standard ML**, built on the positioned
[`sml-mlast`](https://github.com/sjqtentacles/sml-mlast) frontend. It is a
**syntax/scope-level** analyzer: every rule works from the parse tree and the
lexical token stream alone — no type inference, no I/O, no global state. Type-
aware lints (exhaustiveness with full coverage, unused-but-typed values, …) are
deliberately out of scope; they belong to a type elaborator.

Diagnostics are **returned, never raised** (the sole exception is a genuine
lex/parse failure, surfaced cleanly as `Lint.Parse`). Output is deterministic
and stably sorted, so the same input always yields the same report under
**MLton** and **Poly/ML**.

Every diagnostic carries the offending node's **source span** (0-based
line/column, end-exclusive, exactly as `sml-mlast`'s `Pos` produces it), a
severity, a stable rule id, a message, and an optional suggested fix.

## Design principle: precision over recall

Linters are only useful if you trust them. Each rule here is tuned to **avoid
false positives**: when a fact cannot be established from the parse tree alone,
the rule stays silent rather than guessing. This is most visible in
`dead-open` (very conservative) and `nonexhaustive` (only datatypes declared in
the same file). The heuristics and their honest limits are documented per rule
below.

## Rules

| Rule | Severity | What it flags |
|------|----------|---------------|
| `unused` | warning / hint | a value binding never referenced in its scope |
| `shadow` | warning | a binding whose name is already in scope |
| `dead-open` | hint | an `open M` with no qualified or free reference to `M` |
| `nonexhaustive` | warning | a `case`/`fn` over an in-file datatype missing constructors |
| `redundant-parens` | hint | trivially redundant grouping parentheses |
| `naming` | hint | value/constructor names breaking the casing convention |

### `unused`
A `let`/`local` value or function binding, or a `fn`/`fun`/`case` pattern
variable, that is never referenced within its scope. Because the whole scope is
visible, this is sound w.r.t. false positives — it only fires when the name
never occurs as a use anywhere in scope. **Top-level bindings are treated as
exported and never flagged.** Let/local value bindings are `warning`; pattern
variables and parameters are `hint`.

*Suppression:* `sml-mlast`'s lexer forbids leading underscores in identifiers
(a bare `_` is the wildcard pattern), so the idiomatic way to silence this rule
is to bind with the wildcard `_`. A name beginning with `_` is also treated as
intentionally-unused, should the dialect ever admit such names.

### `shadow`
A binding whose name is already in scope from an *enclosing or earlier* binding
in the analyzed source. Basis/imported names are **not** tracked, so shadowing
`map`, `List`, etc. is never reported — only names you defined yourself.

### `dead-open`
An `open M` whose imported names are provably unused. It fires only when the
remainder of the open's scope contains **neither** a qualified `M.x` reference
**nor** any free (otherwise-unbound, non-basis) identifier that could have come
from `M`. Since an `open` introduces unknown names, this is intentionally
conservative: any unresolved bare identifier (including a basis function not in
the built-in pervasive list, or a capitalised name that might be an imported
constructor) keeps the open alive. High precision, deliberately limited recall.

### `redundant-parens`
The `sml-mlast` parser drops grouping parentheses, so this rule works on the
**token stream** instead of the AST. It flags two trivially-redundant shapes:
an atom in parentheses — `( x )`, `( 42 )` — and directly doubled parentheses —
`(( e ))`. Parentheses that disambiguate application, infix, or tuples are never
touched.

### `nonexhaustive`
A best-effort exhaustiveness check. It fires only for a `case`/`fn` whose arms
are **all** constructors of a single datatype **declared in the same file**,
with no wildcard or variable catch-all, that miss at least one constructor (the
missing ones are listed in declaration order). Lists, records, literals, tuples,
and out-of-file/basis types are not analyzed — full exhaustiveness needs type
information, which is out of scope here.

### `naming`
A fixed, documented convention: value and function names should start with a
lower-case letter; datatype constructors and exceptions should start with an
upper-case letter. Only names **defined in the analyzed source** are checked.

## API

```sml
signature LINT =
sig
  type pos  = Pos.pos      (* { line : int, col : int }, 0-based *)
  type span = Pos.span     (* { lo : pos, hi : pos }, end-exclusive *)

  datatype severity = Error | Warning | Hint

  type diagnostic =
    { span : span, severity : severity, rule : string
    , message : string, fix : string option }

  exception Parse of string

  val rules : string list
  val severityToString : severity -> string
  val diagToString     : diagnostic -> string   (* "l0:c0-l1:c1 [sev] rule: msg" *)

  (* per-rule entry points over an already-parsed program (unsorted) *)
  val unusedBindings  : Ast.program -> diagnostic list
  val shadowing       : Ast.program -> diagnostic list
  val deadOpen        : Ast.program -> diagnostic list
  val nonExhaustive   : Ast.program -> diagnostic list
  val naming          : Ast.program -> diagnostic list
  val redundantParens : string      -> diagnostic list   (* needs the token stream *)

  val lintProgram : Ast.program -> diagnostic list        (* all AST rules, unsorted *)
  val lint        : string      -> diagnostic list        (* parse + all rules, sorted *)
  val report      : string      -> string                 (* human-readable report *)
end
```

`lint` parses the source, runs every rule, and returns diagnostics sorted by
span, then severity, then rule, then message. `lintProgram` runs the five AST
rules on a program you already parsed (the token-level `redundantParens` takes
the source string directly). `report` renders a multi-line summary and never
raises — a parse failure becomes a `parse error: …` line.

## Example

Running [`examples/demo.sml`](examples/demo.sml) with `make example` lints a
deliberately messy module and prints:

```
--- source under lint ---
open Helpers

datatype shape = circle of int | Square of int | Triangle of int

fun area s =
  case s of
      circle r => r * r * 3
    | Square w => w * w

fun describe size =
  let
    val size = size * 2
    val label = "shape"
  in
    area (Square (size))
  end

--- lint report ---
0:0-0:12 [hint] dead-open: unused open of `Helpers` (no qualified or free reference to it)
2:0-2:64 [hint] naming: constructor `circle` should start with an upper-case letter
5:2-7:23 [warning] nonexhaustive: non-exhaustive match on datatype `shape`: missing `Triangle`
11:8-11:12 [warning] shadow: binding of `size` shadows an earlier binding
12:8-12:13 [warning] unused: unused value `label`
14:17-14:23 [hint] redundant-parens: redundant parentheses around `size`

6 issue(s): 0 error(s), 3 warning(s), 3 hint(s)
```

One snippet, all six rules: the `open Helpers` is never used; the constructor
`circle` is lower-case; the `case` is missing `Triangle`; the inner `val size`
shadows the parameter `size`; `label` is bound but never used; and the
parentheses around `size` are redundant.

## Build & test

Requires [MLton](http://mlton.org/) and/or [Poly/ML](https://polyml.org/).

```sh
make test        # build + run the suite under MLton
make test-poly   # run the suite under Poly/ML
make all-tests   # both
make example     # build + run the demo
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-lint
smlpkg sync
```

Reference `src/lint.mlb` from your own `.mlb` (MLton / MLKit), or feed
`test/sources.mlb` / your own basis to `tools/polybuild` (Poly/ML). The
`sml-mlast` frontend is vendored under `lib/` so the library is self-contained.

## Layout

```
sml.pkg                                       smlpkg manifest (requires sml-mlast)
Makefile                                      MLton + Poly/ML targets
.github/workflows/ci.yml                      CI: MLton + Poly/ML (variant A)
src/
  lint.sig     LINT signature (diagnostic API + per-rule docs)
  lint.sml     the six rules, the analyzer, sorting + report
  lint.mlb     vendored sml-mlast, then the linter
lib/github.com/sjqtentacles/sml-mlast/        vendored frontend (lexer/parser/AST)
examples/
  demo.sml     lint a messy module, print the report
test/
  harness.sml  shared assertion harness
  test.sml     golden vectors: one snippet per rule (47 checks)
  entry.sml / main.sml
tools/polybuild  Poly/ML build wrapper
```

## Tests

47 deterministic checks. Each rule has positive snippets (asserting the exact
rule id, severity, message AND span via `diagToString`) and negative snippets
(asserting clean source produces no diagnostics), plus coverage of the per-rule
entry points, deterministic span ordering, the rendered `report`, and clean
surfacing of parse failures as `Lint.Parse`. Run `make all-tests` to verify the
output is byte-identical under MLton and Poly/ML.

## License

MIT. See [LICENSE](LICENSE).
