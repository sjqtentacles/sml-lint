(* lexer.sig

   Hand-written scanner from a source string to a list of positioned tokens
   (each Token.token paired with its source span; terminated by a Token.EOF
   token whose span is the empty range at end-of-input). Handles nested
   (* ... *) comments, qualified identifiers (A.B.c as a single ID), symbolic
   identifiers, tyvars, integer/word/real literals (spelling preserved verbatim)
   and string/char literals (decoded to their payload).

   Line/column are tracked correctly across newlines, nested comments and
   string/char escapes. Positions are 0-based and spans are end-exclusive (see
   Pos). Raises Lex on malformed input. Pure and deterministic. *)

signature LEXER =
sig
  exception Lex of string
  type ptoken = Token.token * Pos.span

  val tokenize    : string -> ptoken list
  (* convenience: drop spans, recovering the old token-only view *)
  val tokensOnly  : string -> Token.token list
end
