(* token.sml - see token.sig *)

structure Token :> TOKEN =
struct
  datatype token =
      ABSTYPE | AND | ANDALSO | AS | CASE | DATATYPE | DO | ELSE | END
    | EXCEPTION | FN | FUN | HANDLE | IF | IN | INFIX | INFIXR | LET | LOCAL
    | NONFIX | OF | OP | OPEN | ORELSE | RAISE | REC | THEN | TYPE | VAL
    | WITH | WITHTYPE | WHILE
    | EQTYPE | FUNCTOR | INCLUDE | SHARING | SIG | SIGNATURE | STRUCT
    | STRUCTURE | WHERE
    | LPAREN | RPAREN | LBRACK | RBRACK | LBRACE | RBRACE
    | COMMA | COLON | SEMICOLON | DOTDOTDOT | UNDERSCORE | BAR | EQUALS
    | DARROW | ARROW | HASH | COLONGT
    | ID of string
    | TYVAR of string
    | INT of string
    | WORD of string
    | REAL of string
    | STRING of string
    | CHAR of string
    | EOF

  fun toString t =
    case t of
        ABSTYPE => "abstype" | AND => "and" | ANDALSO => "andalso"
      | AS => "as" | CASE => "case" | DATATYPE => "datatype" | DO => "do"
      | ELSE => "else" | END => "end" | EXCEPTION => "exception" | FN => "fn"
      | FUN => "fun" | HANDLE => "handle" | IF => "if" | IN => "in"
      | INFIX => "infix" | INFIXR => "infixr" | LET => "let" | LOCAL => "local"
      | NONFIX => "nonfix" | OF => "of" | OP => "op" | OPEN => "open"
      | ORELSE => "orelse" | RAISE => "raise" | REC => "rec" | THEN => "then"
      | TYPE => "type" | VAL => "val" | WITH => "with" | WITHTYPE => "withtype"
      | WHILE => "while" | EQTYPE => "eqtype" | FUNCTOR => "functor"
      | INCLUDE => "include" | SHARING => "sharing" | SIG => "sig"
      | SIGNATURE => "signature" | STRUCT => "struct" | STRUCTURE => "structure"
      | WHERE => "where"
      | LPAREN => "(" | RPAREN => ")" | LBRACK => "[" | RBRACK => "]"
      | LBRACE => "{" | RBRACE => "}" | COMMA => "," | COLON => ":"
      | SEMICOLON => ";" | DOTDOTDOT => "..." | UNDERSCORE => "_"
      | BAR => "|" | EQUALS => "=" | DARROW => "=>" | ARROW => "->"
      | HASH => "#" | COLONGT => ":>"
      | ID s => s | TYVAR s => s | INT s => s | WORD s => s | REAL s => s
      | STRING s => "\"" ^ s ^ "\"" | CHAR s => "#\"" ^ s ^ "\""
      | EOF => "<eof>"

  fun sameToken (a, b) = (a = b)
end
