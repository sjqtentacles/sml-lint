(* Tests for sml-lint: golden vectors over small SML source snippets, each
   crafted to trigger (or deliberately NOT trigger) one rule. Diagnostics are
   compared as their canonical `diagToString` rendering, which pins down rule,
   severity, message AND span. Reference values were computed by hand against
   the snippets and verified against both compilers. *)

structure Tests =
struct
  open Harness

  (* full pipeline: parse + all rules, sorted *)
  fun diags src = List.map Lint.diagToString (Lint.lint src)

  (* a single program-level rule, run on a freshly parsed program *)
  fun pdiags rule src = List.map Lint.diagToString (rule (Parser.parseString src))

  fun runAll () =
    let
      val () = section "meta"
      val () = checkStringList "rule ids"
        (["unused", "shadow", "dead-open", "nonexhaustive",
          "redundant-parens", "naming"], Lint.rules)
      val () = checkString "severity error"   ("error",   Lint.severityToString Lint.Error)
      val () = checkString "severity warning" ("warning", Lint.severityToString Lint.Warning)
      val () = checkString "severity hint"    ("hint",    Lint.severityToString Lint.Hint)

      (* ---------------- rule: unused ---------------- *)
      val () = section "unused: let value"
      val () = checkStringList "let-bound value never used"
        (["0:16-0:17 [warning] unused: unused value `x`"],
         diags "val r = let val x = 1 in 2 end")
      val () = checkStringList "let-bound value used -> clean"
        ([], diags "val r = let val x = 1 in x + 1 end")

      val () = section "unused: let function"
      val () = checkStringList "let-bound fun never used"
        (["0:12-0:23 [warning] unused: unused function `g`"],
         diags "val r = let fun g x = x in 2 end")

      val () = section "unused: local"
      val () = checkStringList "local private binding never used in body"
        (["0:10-0:16 [warning] unused: unused value `secret`"],
         diags "local val secret = 1 in val pub = 2 end")

      val () = section "unused: parameters"
      val () = checkStringList "unused fun parameter (hint)"
        (["0:6-0:7 [hint] unused: unused parameter `x`"],
         diags "fun f x = 1")
      val () = checkStringList "used parameters -> clean"
        ([], diags "fun f (x, y) = x + y")
      val () = checkStringList "unused case-arm variable (hint)"
        (["0:24-0:25 [hint] unused: unused variable `b`"],
         diags "fun f p = case p of (a, b) => a")

      val () = section "unused: top-level not flagged (treated as exported)"
      val () = checkStringList "top-level val never flagged"
        ([], diags "val publicValue = 42")
      val () = checkStringList "wildcard suppresses unused"
        ([], diags "val r = let val _ = 1 in 2 end")

      (* ---------------- rule: shadow ---------------- *)
      val () = section "shadow"
      val () = checkStringList "inner binding shadows outer of same name"
        (["1:16-1:17 [warning] shadow: binding of `x` shadows an earlier binding"],
         diags "val x = 1\nval y = let val x = 2 in x + x end")
      val () = checkStringList "distinct scopes do not shadow"
        ([], diags "fun f x = x\nfun g x = x")
      val () = checkStringList "shadowing the basis is not reported"
        ([], diags "val r = let val map = 5 in map end")

      (* ---------------- rule: dead-open ---------------- *)
      val () = section "dead-open"
      val () = checkStringList "open never referenced"
        (["0:0-0:8 [hint] dead-open: unused open of `Foo` (no qualified or free reference to it)"],
         diags "open Foo\nval x = 3")
      val () = checkStringList "qualified use keeps open alive"
        ([], diags "open Foo\nval x = Foo.bar 1")
      val () = checkStringList "free bare identifier suppresses dead-open"
        ([], diags "open Foo\nval x = bar 1")
      val () = checkStringList "multi-open flags only the unused one"
        (["0:0-0:8 [hint] dead-open: unused open of `B` (no qualified or free reference to it)"],
         diags "open A B\nval z = A.x")

      (* ---------------- rule: nonexhaustive ---------------- *)
      val () = section "nonexhaustive"
      val () = checkStringList "case missing one constructor"
        (["1:10-1:41 [warning] nonexhaustive: non-exhaustive match on datatype `color`: missing `Blue`"],
         diags "datatype color = Red | Green | Blue\nfun f c = case c of Red => 1 | Green => 2")
      val () = checkStringList "case missing several constructors (declared order)"
        (["1:10-1:26 [warning] nonexhaustive: non-exhaustive match on datatype `c`: missing `B`, `C`, `D`"],
         diags "datatype c = A | B | C | D\nfun f x = case x of A => 1")
      val () = checkStringList "fn missing a constructor"
        (["1:9-1:18 [warning] nonexhaustive: non-exhaustive match on datatype `d`: missing `R`"],
         diags "datatype d = L | R\nval f = (fn L => 1)")
      val () = checkStringList "exhaustive case -> clean"
        ([], diags "datatype c = A | B\nfun f x = case x of A => 1 | B => 2")
      val () = checkStringList "wildcard catch-all -> clean"
        ([], diags "datatype c = A | B | C\nfun f x = case x of A => 1 | _ => 2")
      val () = checkStringList "unknown (out-of-file) datatype not analyzed"
        ([], diags "fun f x = case x of SOME y => y | NONE => 0")

      (* ---------------- rule: redundant-parens ---------------- *)
      val () = section "redundant-parens"
      val () = checkStringList "atom in parens"
        (["0:8-0:11 [hint] redundant-parens: redundant parentheses around `1`"],
         diags "val x = (1)")
      val () = checkStringList "doubled parens"
        (["0:8-0:17 [hint] redundant-parens: redundant nested parentheses"],
         diags "val y = ((1 + 2))")
      val () = checkStringList "necessary parens -> clean"
        ([], diags "val x = (1 + 2) * 3")
      val () = checkStringList "tuple parens are not redundant"
        ([], diags "val p = (1, 2)")

      (* ---------------- rule: naming ---------------- *)
      val () = section "naming"
      val () = checkStringList "capitalised function name"
        (["0:0-0:17 [hint] naming: function `Foo` should start with a lower-case letter"],
         diags "fun Foo x = x + 1")
      val () = checkStringList "capitalised value name"
        (["0:4-0:9 [hint] naming: value `Thing` should start with a lower-case letter"],
         diags "val Thing = 1")
      val () = checkStringList "lower-case constructor name"
        (["0:0-0:23 [hint] naming: constructor `bad` should start with an upper-case letter"],
         diags "datatype t = bad | Good")
      val () = checkStringList "lower-case exception name"
        (["0:0-0:14 [hint] naming: exception `oops` should start with an upper-case letter"],
         diags "exception oops")
      val () = checkStringList "conventional names -> clean"
        ([], diags "fun add x y = x + y\ndatatype rgb = Red | Green")

      (* ---------------- determinism / sorting / report ---------------- *)
      val () = section "ordering and report"
      val () = checkStringList "diagnostics sorted by span then severity then rule"
        (["0:0-0:8 [hint] dead-open: unused open of `Sys` (no qualified or free reference to it)",
          "1:0-1:41 [hint] naming: function `Go` should start with a lower-case letter",
          "1:19-1:26 [warning] unused: unused value `unusedv`",
          "1:29-1:32 [hint] redundant-parens: redundant parentheses around `n`"],
         diags "open Sys\nfun Go n = let val unusedv = (n) in 1 end")
      val () = check "lint is deterministic across runs"
        (diags "open Sys\nfun Go n = let val unusedv = (n) in 1 end"
         = diags "open Sys\nfun Go n = let val unusedv = (n) in 1 end")
      val () = checkString "report for a single hint"
        ("0:8-0:11 [hint] redundant-parens: redundant parentheses around `1`\n\n"
         ^ "1 issue(s): 0 error(s), 0 warning(s), 1 hint(s)\n",
         Lint.report "val x = (1)")
      val () = checkString "report for clean source"
        ("No issues found.\n\n0 issue(s): 0 error(s), 0 warning(s), 0 hint(s)\n",
         Lint.report "fun add x y = x + y")

      (* ---------------- per-rule entry points ---------------- *)
      val () = section "per-rule entry points"
      val () = checkStringList "unusedBindings"
        (["0:16-0:17 [warning] unused: unused value `x`"],
         pdiags Lint.unusedBindings "val r = let val x = 1 in 2 end")
      val () = checkStringList "shadowing"
        (["1:16-1:17 [warning] shadow: binding of `x` shadows an earlier binding"],
         pdiags Lint.shadowing "val x = 1\nval y = let val x = 2 in x + x end")
      val () = checkStringList "deadOpen"
        (["0:0-0:8 [hint] dead-open: unused open of `Foo` (no qualified or free reference to it)"],
         pdiags Lint.deadOpen "open Foo\nval x = 3")
      val () = checkStringList "nonExhaustive"
        (["1:10-1:41 [warning] nonexhaustive: non-exhaustive match on datatype `color`: missing `Blue`"],
         pdiags Lint.nonExhaustive
           "datatype color = Red | Green | Blue\nfun f c = case c of Red => 1 | Green => 2")
      val () = checkStringList "naming"
        (["0:0-0:17 [hint] naming: function `Foo` should start with a lower-case letter"],
         pdiags Lint.naming "fun Foo x = x + 1")
      val () = checkStringList "redundantParens (source-level)"
        (["0:8-0:11 [hint] redundant-parens: redundant parentheses around `1`"],
         List.map Lint.diagToString (Lint.redundantParens "val x = (1)"))

      (* ---------------- parse failures surfaced as Parse ---------------- *)
      val () = section "parse failures"
      val () = check "genuine parse error raises Lint.Parse"
        ((let val _ = Lint.lint "val 1 = = =" in false end)
         handle Lint.Parse _ => true | _ => false)
      val () = check "report swallows parse errors cleanly"
        (String.isPrefix "parse error:" (Lint.report "val 1 = = ="))
    in
      Harness.run ()
    end

  val run = runAll
end
