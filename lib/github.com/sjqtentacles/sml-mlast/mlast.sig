(* mlast.sig

   Convenience umbrella over the frontend: lex+parse a source string to an Ast,
   pretty-print an Ast back to source, and the composed `format`. `idempotent`
   is the round-trip property used by the test corpus. *)

signature MLAST =
sig
  val parse      : string -> Ast.program
  val pp         : Ast.program -> string
  val format     : string -> string        (* pp o parse *)
  val idempotent : string -> bool           (* format (format s) = format s *)
end
