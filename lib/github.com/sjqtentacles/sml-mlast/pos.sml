(* pos.sml - see pos.sig *)

structure Pos :> POS =
struct
  type pos = { line : int, col : int }
  type span = { lo : pos, hi : pos }

  val origin : pos = { line = 0, col = 0 }
  val zero : span = { lo = origin, hi = origin }

  fun mkPos (line, col) = { line = line, col = col }
  fun mkSpan (lo, hi) = { lo = lo, hi = hi }

  fun merge (a : span, b : span) = { lo = #lo a, hi = #hi b }

  fun posToString ({ line, col } : pos) =
    Int.toString line ^ ":" ^ Int.toString col

  fun spanToString ({ lo, hi } : span) =
    posToString lo ^ "-" ^ posToString hi

  fun samePos (a : pos, b : pos) =
    #line a = #line b andalso #col a = #col b

  fun sameSpan (a : span, b : span) =
    samePos (#lo a, #lo b) andalso samePos (#hi a, #hi b)
end
