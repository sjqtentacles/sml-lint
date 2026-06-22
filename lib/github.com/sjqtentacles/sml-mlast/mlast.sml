(* mlast.sml - see mlast.sig *)

structure Mlast :> MLAST =
struct
  fun parse s = Parser.parseString s
  fun pp ds = PpAst.ppProgram ds
  fun format s = pp (parse s)
  fun idempotent s = let val a = format s in format a = a end
end
