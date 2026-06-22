(* lint.sig

   A pure, deterministic static linter over the positioned sml-mlast AST. It is
   a SYNTAX/SCOPE-level analyzer: every rule works from the parse tree and the
   lexical token stream alone (no type inference). Type-aware lints are out of
   scope (they belong to sml-elab).

   Each rule emits `diagnostic`s carrying the offending node's source `span`
   (0-based line/col, end-exclusive, exactly as produced by sml-mlast's Pos).
   Diagnostics are RETURNED, never raised; the single exception is a genuine
   lex/parse failure, surfaced cleanly as `Parse`.

   `lint` is the umbrella entry point: it parses a source string, runs every
   rule, and returns the diagnostics in a deterministic, stable order (sorted by
   span, then severity, then rule name, then message). The same input always
   produces the same output under MLton and Poly/ML.

   The six rules and their (documented) heuristics:

   - "unused"          a value binding (a `let`/`local` val/fun binding, or a
                       fn/fun/case pattern variable) that is never referenced in
                       its scope. Sound w.r.t. false positives: it only fires
                       when the whole scope is visible and the name never occurs
                       as a variable use anywhere in it.
   - "shadow"          a binding whose name is already in scope (bound earlier in
                       the analyzed source). Basis/imported names are NOT tracked,
                       so shadowing `map`, `List`, etc. is never reported.
   - "dead-open"       an `open M` whose imported names are provably unused: the
                       remainder of its scope contains neither a qualified `M.x`
                       reference nor ANY free (otherwise-unbound) bare identifier
                       that could have come from `M`. Conservative by design.
   - "redundant-parens" trivially redundant grouping parentheses detected on the
                       token stream (the AST drops grouping parens): an atom in
                       parens `( x )` / `( 42 )`, or doubled parens `(( e ))`.
   - "nonexhaustive"   a `case`/`fn` match over a datatype DECLARED IN THE SAME
                       FILE whose arms are all constructors of that datatype, miss
                       at least one constructor, and have no wildcard/variable
                       catch-all. Lists/records/literals and out-of-file types are
                       not analyzed (full exhaustiveness needs type info).
   - "naming"          value/function names should start lower-case; constructor
                       and exception names should start upper-case. Only names
                       DEFINED in the analyzed source are checked.

   Note on the "intentionally unused" convention: sml-mlast's lexer forbids
   leading underscores in identifiers (a bare `_` is the wildcard pattern), so
   the idiomatic way to silence "unused" is the wildcard `_`. A name beginning
   with `_` is nonetheless treated as intentionally-unused should the dialect
   ever admit it. *)

signature LINT =
sig
  (* re-exported position types from the vendored sml-mlast *)
  type pos  = Pos.pos      (* { line : int, col : int }, 0-based *)
  type span = Pos.span     (* { lo : pos, hi : pos }, end-exclusive *)

  datatype severity = Error | Warning | Hint

  type diagnostic =
    { span     : span
    , severity : severity
    , rule     : string         (* stable rule id, e.g. "unused" *)
    , message  : string
    , fix      : string option  (* a suggested replacement, when obvious *)
    }

  (* genuine lex/parse failure, surfaced cleanly (diagnostics are never raised) *)
  exception Parse of string

  (* the stable list of rule ids this linter knows about *)
  val rules : string list

  val severityToString : severity -> string
  (* "l0:c0-l1:c1 [severity] rule: message" *)
  val diagToString     : diagnostic -> string

  (* per-rule entry points over an already-parsed program (unsorted) *)
  val unusedBindings   : Ast.program -> diagnostic list
  val shadowing        : Ast.program -> diagnostic list
  val deadOpen         : Ast.program -> diagnostic list
  val nonExhaustive    : Ast.program -> diagnostic list
  val naming           : Ast.program -> diagnostic list
  (* redundant-parens needs the token stream, so it takes the source string *)
  val redundantParens  : string -> diagnostic list

  (* run every program-level rule on a parsed program (unsorted) *)
  val lintProgram : Ast.program -> diagnostic list

  (* parse `source`, run all rules, return diagnostics sorted deterministically.
     Raises `Parse` only on a genuine lex/parse failure. *)
  val lint : string -> diagnostic list

  (* a multi-line human-readable report: one diagnostic per line, then a summary
     count. Deterministic; this is what the demo prints. *)
  val report : string -> string
end
