(* token.sig

   Lexical tokens for the SML'97 frontend. Identifiers carry their (possibly
   qualified) spelling; numeric/string/char constants carry a normalised
   payload (see Lexer) so that the pretty-printer can re-emit them canonically.

   The datatype is exposed transparently: consumers pattern-match on tokens. *)

signature TOKEN =
sig
  datatype token =
      (* reserved words - Core *)
      ABSTYPE | AND | ANDALSO | AS | CASE | DATATYPE | DO | ELSE | END
    | EXCEPTION | FN | FUN | HANDLE | IF | IN | INFIX | INFIXR | LET | LOCAL
    | NONFIX | OF | OP | OPEN | ORELSE | RAISE | REC | THEN | TYPE | VAL
    | WITH | WITHTYPE | WHILE
      (* reserved words - Modules *)
    | EQTYPE | FUNCTOR | INCLUDE | SHARING | SIG | SIGNATURE | STRUCT
    | STRUCTURE | WHERE
      (* punctuation / reserved symbols *)
    | LPAREN | RPAREN | LBRACK | RBRACK | LBRACE | RBRACE
    | COMMA | COLON | SEMICOLON | DOTDOTDOT | UNDERSCORE | BAR | EQUALS
    | DARROW            (* => *)
    | ARROW             (* -> *)
    | HASH              (* #  *)
    | COLONGT           (* :> *)
      (* identifiers and constants *)
    | ID of string      (* alphanumeric or symbolic vid/longvid spelling *)
    | TYVAR of string    (* 'a, ''b *)
    | INT of string      (* integer constant, normalised spelling *)
    | WORD of string     (* word constant: 0w... *)
    | REAL of string     (* real constant, normalised spelling *)
    | STRING of string   (* decoded string contents (no quotes/escapes) *)
    | CHAR of string     (* decoded char contents (single character) *)
    | EOF

  val toString : token -> string
  val sameToken : token * token -> bool
end
