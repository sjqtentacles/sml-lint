(* pos.sig

   Source positions and spans for the SML frontend.

   A `pos` is a zero-based (line, col) coordinate, matching the LSP convention
   (Language Server Protocol positions are 0-based for both line and character).
   A `span` is a half-open range [lo, hi): `lo` is the position of the first
   character of a token/node and `hi` is the position one past its last
   character (again matching LSP, whose ranges are end-exclusive).

   The types are exposed transparently as records so that consumers can pattern
   match / build them directly, and so that {line,col}/{lo,hi} records produced
   anywhere unify structurally. Pure and deterministic. *)

signature POS =
sig
  type pos = { line : int, col : int }
  type span = { lo : pos, hi : pos }

  val origin : pos                       (* {line=0, col=0} *)
  val zero   : span                      (* {lo=origin, hi=origin} *)

  val mkPos  : int * int -> pos          (* (line, col) -> pos *)
  val mkSpan : pos * pos -> span         (* (lo, hi) -> span *)

  (* Merge two spans into the span covering both (lo of the first, hi of the
     second); used by the parser to combine sub-node spans. *)
  val merge  : span * span -> span

  val posToString  : pos -> string       (* "line:col" *)
  val spanToString : span -> string      (* "l0:c0-l1:c1" *)

  val samePos  : pos * pos -> bool
  val sameSpan : span * span -> bool
end
