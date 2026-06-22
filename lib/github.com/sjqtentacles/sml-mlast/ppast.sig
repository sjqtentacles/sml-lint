(* ppast.sig

   Deterministic pretty-printer from Ast back to SML source text. Layout is
   fixed (2-space block indentation; infix/typed/andalso/orelse expressions are
   fully parenthesised; declarations one per line), which is what guarantees
   the round-trip property parse (pp (parse s)) = parse s, i.e. pp is a fixed
   point after the first pass. Pure and deterministic. *)

signature PPAST =
sig
  val ppTy      : Ast.ty -> string
  val ppPat     : Ast.pat -> string
  val ppExp     : Ast.exp -> string
  val ppDec     : Ast.dec -> string
  val ppProgram : Ast.program -> string
end
