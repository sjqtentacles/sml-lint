(* lexer.sml - see lexer.sig *)

structure Lexer :> LEXER =
struct
  exception Lex of string
  type ptoken = Token.token * Pos.span

  fun isSym c = Char.contains "!%&$#+-/:<=>?@\\~`^|*" c
  fun isIdChar c = Char.isAlphaNum c orelse c = #"_" orelse c = #"'"

  fun reserved w =
    case w of
        "abstype" => SOME Token.ABSTYPE
      | "and" => SOME Token.AND
      | "andalso" => SOME Token.ANDALSO
      | "as" => SOME Token.AS
      | "case" => SOME Token.CASE
      | "datatype" => SOME Token.DATATYPE
      | "do" => SOME Token.DO
      | "else" => SOME Token.ELSE
      | "end" => SOME Token.END
      | "exception" => SOME Token.EXCEPTION
      | "fn" => SOME Token.FN
      | "fun" => SOME Token.FUN
      | "handle" => SOME Token.HANDLE
      | "if" => SOME Token.IF
      | "in" => SOME Token.IN
      | "infix" => SOME Token.INFIX
      | "infixr" => SOME Token.INFIXR
      | "let" => SOME Token.LET
      | "local" => SOME Token.LOCAL
      | "nonfix" => SOME Token.NONFIX
      | "of" => SOME Token.OF
      | "op" => SOME Token.OP
      | "open" => SOME Token.OPEN
      | "orelse" => SOME Token.ORELSE
      | "raise" => SOME Token.RAISE
      | "rec" => SOME Token.REC
      | "then" => SOME Token.THEN
      | "type" => SOME Token.TYPE
      | "val" => SOME Token.VAL
      | "with" => SOME Token.WITH
      | "withtype" => SOME Token.WITHTYPE
      | "while" => SOME Token.WHILE
      | "eqtype" => SOME Token.EQTYPE
      | "functor" => SOME Token.FUNCTOR
      | "include" => SOME Token.INCLUDE
      | "sharing" => SOME Token.SHARING
      | "sig" => SOME Token.SIG
      | "signature" => SOME Token.SIGNATURE
      | "struct" => SOME Token.STRUCT
      | "structure" => SOME Token.STRUCTURE
      | "where" => SOME Token.WHERE
      | _ => NONE

  fun tokenize s =
    let
      val n = String.size s
      fun has i = i < n
      fun ch i = String.sub (s, i)
      fun readWhile (i, pred) =
        if has i andalso pred (ch i) then readWhile (i + 1, pred) else i

      (* line-start offsets: lineStarts[k] is the char index at which (0-based)
         line k begins. Used to map a char index to a {line, col} position. *)
      val lineStarts =
        let
          fun go (i, acc) =
            if i >= n then List.rev acc
            else if ch i = #"\n" then go (i + 1, (i + 1) :: acc)
            else go (i + 1, acc)
        in Vector.fromList (0 :: go (0, [])) end
      val numLines = Vector.length lineStarts

      fun posOf idx =
        let
          (* largest k with lineStarts[k] <= idx (binary search) *)
          fun bsearch (lo, hi) =
            if lo >= hi then lo
            else
              let val mid = (lo + hi + 1) div 2
              in if Vector.sub (lineStarts, mid) <= idx
                 then bsearch (mid, hi)
                 else bsearch (lo, mid - 1)
              end
          val line = bsearch (0, numLines - 1)
        in { line = line, col = idx - Vector.sub (lineStarts, line) } : Pos.pos end

      fun spanOf (a, b) = { lo = posOf a, hi = posOf b } : Pos.span

      fun skipComment (i, depth) =
        if depth = 0 then i
        else if not (has i) then raise Lex "unterminated comment"
        else if has (i + 1) andalso ch i = #"(" andalso ch (i + 1) = #"*"
          then skipComment (i + 2, depth + 1)
        else if has (i + 1) andalso ch i = #"*" andalso ch (i + 1) = #")"
          then skipComment (i + 2, depth - 1)
        else skipComment (i + 1, depth)

      fun skipGap j =
        if has j andalso Char.isSpace (ch j) then skipGap (j + 1)
        else if has j andalso ch j = #"\\" then j + 1
        else raise Lex "malformed string gap"

      (* escape: index just past the backslash -> (char option, next index);
         NONE means an elided "\ ... \" whitespace gap. *)
      fun escape j =
        if not (has j) then raise Lex "bad escape" else
        case ch j of
            #"n" => (SOME #"\n", j + 1)
          | #"t" => (SOME #"\t", j + 1)
          | #"r" => (SOME (Char.chr 13), j + 1)
          | #"f" => (SOME (Char.chr 12), j + 1)
          | #"a" => (SOME (Char.chr 7), j + 1)
          | #"b" => (SOME (Char.chr 8), j + 1)
          | #"v" => (SOME (Char.chr 11), j + 1)
          | #"\\" => (SOME #"\\", j + 1)
          | #"\"" => (SOME #"\"", j + 1)
          | #"^" =>
              if has (j + 1) then (SOME (Char.chr (Char.ord (ch (j + 1)) - 64)), j + 2)
              else raise Lex "bad control escape"
          | c =>
              if Char.isDigit c then
                if has (j + 2) then
                  case Int.fromString (String.substring (s, j, 3)) of
                      SOME code => (SOME (Char.chr code), j + 3)
                    | NONE => raise Lex "bad numeric escape"
                else raise Lex "bad numeric escape"
              else if Char.isSpace c then (NONE, skipGap j)
              else raise Lex ("bad escape: \\" ^ String.str c)

      fun lexString i =
        let
          fun go (j, acc) =
            if not (has j) then raise Lex "unterminated string"
            else case ch j of
                #"\"" => (Token.STRING (String.implode (List.rev acc)), j + 1)
              | #"\\" =>
                  let val (co, k) = escape (j + 1)
                  in case co of NONE => go (k, acc)
                              | SOME c => go (k, c :: acc) end
              | c => go (j + 1, c :: acc)
        in go (i, []) end

      fun lexChar i =
        if not (has i) then raise Lex "unterminated char" else
        case ch i of
            #"\\" =>
              let val (co, k) = escape (i + 1)
              in case co of
                     SOME c => if has k andalso ch k = #"\""
                               then (Token.CHAR (String.str c), k + 1)
                               else raise Lex "bad char literal"
                   | NONE => raise Lex "bad char literal"
              end
          | #"\"" => raise Lex "empty char literal"
          | c => if has (i + 1) andalso ch (i + 1) = #"\""
                 then (Token.CHAR (String.str c), i + 2)
                 else raise Lex "bad char literal"

      fun lexNumber i =
        let
          val start = i
          val i = if ch i = #"~" then i + 1 else i
        in
          if has (i + 1) andalso ch i = #"0" andalso ch (i + 1) = #"w" then
            let
              val j0 = i + 2
              val (hex, j1) = if has j0 andalso ch j0 = #"x" then (true, j0 + 1)
                              else (false, j0)
              val j = readWhile (j1, fn c => if hex then Char.isHexDigit c
                                             else Char.isDigit c)
            in (Token.WORD (String.substring (s, start, j - start)), j) end
          else if has (i + 1) andalso ch i = #"0" andalso ch (i + 1) = #"x" then
            let val j = readWhile (i + 2, Char.isHexDigit)
            in (Token.INT (String.substring (s, start, j - start)), j) end
          else
            let
              val j1 = readWhile (i, Char.isDigit)
              val (real2, j2) =
                if has (j1 + 1) andalso ch j1 = #"." andalso Char.isDigit (ch (j1 + 1))
                  then (true, readWhile (j1 + 1, Char.isDigit))
                  else (false, j1)
              val (real3, j3) =
                if has j2 andalso (ch j2 = #"e" orelse ch j2 = #"E") then
                  let
                    val k0 = j2 + 1
                    val k1 = if has k0 andalso (ch k0 = #"~" orelse ch k0 = #"-")
                             then k0 + 1 else k0
                  in
                    if has k1 andalso Char.isDigit (ch k1)
                      then (true, readWhile (k1, Char.isDigit))
                      else (real2, j2)
                  end
                else (real2, j2)
              val mk = if real3 then Token.REAL else Token.INT
            in (mk (String.substring (s, start, j3 - start)), j3) end
        end

      fun lexIdent i =
        let
          fun gather j =
            if has j andalso ch j = #"." andalso has (j + 1)
               andalso (Char.isAlpha (ch (j + 1)) orelse isSym (ch (j + 1)))
              then
                let val k = if Char.isAlpha (ch (j + 1))
                            then readWhile (j + 1, isIdChar)
                            else readWhile (j + 1, isSym)
                in gather k end
              else j
          val e0 = readWhile (i, isIdChar)
          val e = gather e0
          val name = String.substring (s, i, e - i)
        in
          if e = e0 then
            (case reserved name of SOME t => (t, e) | NONE => (Token.ID name, e))
          else (Token.ID name, e)
        end

      (* emit a token with the span covering source range [lo, hi) *)
      fun loop (i, acc) =
        if not (has i) then List.rev ((Token.EOF, spanOf (n, n)) :: acc)
        else
          let
            val c = ch i
            fun emit (t, j) = loop (j, (t, spanOf (i, j)) :: acc)
          in
            if Char.isSpace c then loop (i + 1, acc)
            else if c = #"(" andalso has (i + 1) andalso ch (i + 1) = #"*"
              then loop (skipComment (i + 2, 1), acc)
            else if c = #"(" then emit (Token.LPAREN, i + 1)
            else if c = #")" then emit (Token.RPAREN, i + 1)
            else if c = #"[" then emit (Token.LBRACK, i + 1)
            else if c = #"]" then emit (Token.RBRACK, i + 1)
            else if c = #"{" then emit (Token.LBRACE, i + 1)
            else if c = #"}" then emit (Token.RBRACE, i + 1)
            else if c = #"," then emit (Token.COMMA, i + 1)
            else if c = #";" then emit (Token.SEMICOLON, i + 1)
            else if c = #"_" then emit (Token.UNDERSCORE, i + 1)
            else if c = #"." then
              if has (i + 2) andalso ch (i + 1) = #"." andalso ch (i + 2) = #"."
                then emit (Token.DOTDOTDOT, i + 3)
                else raise Lex "unexpected '.'"
            else if c = #"\"" then emit (lexString (i + 1))
            else if c = #"#" andalso has (i + 1) andalso ch (i + 1) = #"\"" then
              emit (lexChar (i + 2))
            else if Char.isDigit c then emit (lexNumber i)
            else if c = #"~" andalso has (i + 1) andalso Char.isDigit (ch (i + 1)) then
              emit (lexNumber i)
            else if c = #"'" then
              let val j = readWhile (i + 1, isIdChar)
              in emit (Token.TYVAR (String.substring (s, i, j - i)), j) end
            else if Char.isAlpha c then emit (lexIdent i)
            else if isSym c then
              let
                val j = readWhile (i, isSym)
                val sym = String.substring (s, i, j - i)
                val t = case sym of
                            "=" => Token.EQUALS
                          | ":" => Token.COLON
                          | ":>" => Token.COLONGT
                          | "->" => Token.ARROW
                          | "=>" => Token.DARROW
                          | "|" => Token.BAR
                          | "#" => Token.HASH
                          | _ => Token.ID sym
              in emit (t, j) end
            else raise Lex ("illegal character: " ^ String.str c)
          end
    in loop (0, []) end

  fun tokensOnly s = List.map (fn (t, _) => t) (tokenize s)
end
