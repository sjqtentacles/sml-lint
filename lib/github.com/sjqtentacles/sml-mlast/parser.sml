(* parser.sml - see parser.sig *)

structure Parser :> PARSER =
struct
  open Ast
  exception Parse of string

  datatype assoc = LeftA | RightA

  val defaultFixity : (string * (int * assoc)) list =
    [ ("/", (7, LeftA)), ("*", (7, LeftA)), ("div", (7, LeftA)),
      ("mod", (7, LeftA)),
      ("+", (6, LeftA)), ("-", (6, LeftA)), ("^", (6, LeftA)),
      ("::", (5, RightA)), ("@", (5, RightA)),
      ("=", (4, LeftA)), ("<>", (4, LeftA)), ("<", (4, LeftA)),
      (">", (4, LeftA)), ("<=", (4, LeftA)), (">=", (4, LeftA)),
      (":=", (3, LeftA)), ("o", (3, LeftA)),
      ("before", (0, LeftA)) ]

  (* mutable parse state: parallel token / span vectors + a cursor *)
  val toksRef = ref (Vector.fromList ([] : Token.token list))
  val spansRef = ref (Vector.fromList ([] : Pos.span list))
  val posRef = ref 0
  val fixityRef = ref defaultFixity

  fun peek () = Vector.sub (!toksRef, !posRef) handle Subscript => Token.EOF
  fun peekN k = Vector.sub (!toksRef, !posRef + k) handle Subscript => Token.EOF
  fun adv () = posRef := !posRef + 1

  (* span bookkeeping: clamp out-of-range indices to the ends of the vector *)
  fun spanAt i =
    let val m = Vector.length (!spansRef)
    in if m = 0 then Pos.zero
       else if i < 0 then Vector.sub (!spansRef, 0)
       else if i >= m then Vector.sub (!spansRef, m - 1)
       else Vector.sub (!spansRef, i)
    end
  fun curSpan () = spanAt (!posRef)
  fun curLo () = #lo (curSpan ())
  fun prevHi () = #hi (spanAt (!posRef - 1))

  (* wrap a freshly built node: it spans from `lo` to the last consumed token *)
  fun mk lo node = (node, { lo = lo, hi = prevHi () } : span)
  (* extract the lo/span of an already-wrapped sub-node *)
  fun spanOfW (_, sp) = sp
  fun loOfW (_, (sp : span)) = #lo sp
  (* span covering a non-empty list of wrapped sub-nodes *)
  fun spanList ws =
    { lo = loOfW (List.hd ws), hi = #hi (spanOfW (List.last ws)) } : span

  fun expect t =
    if peek () = t then adv ()
    else raise Parse ("expected " ^ Token.toString t ^ ", got "
                      ^ Token.toString (peek ()))

  fun lookupFix s =
    case List.find (fn (k, _) => k = s) (!fixityRef) of
        SOME (_, f) => SOME f
      | NONE => NONE
  fun isInfix s = Option.isSome (lookupFix s)
  fun addFixity (s, prec, assoc) =
    fixityRef := (s, (prec, assoc)) :: List.filter (fn (k, _) => k <> s) (!fixityRef)
  fun removeFixity s =
    fixityRef := List.filter (fn (k, _) => k <> s) (!fixityRef)

  fun peekInfixOp () =
    case peek () of
        Token.ID s => (case lookupFix s of SOME f => SOME (s, f) | NONE => NONE)
      | Token.EQUALS => SOME ("=", (4, LeftA))
      | _ => NONE

  fun atexpStarts t =
    case t of
        Token.ID s => not (isInfix s)
      | Token.OP => true
      | Token.INT _ => true | Token.WORD _ => true | Token.REAL _ => true
      | Token.STRING _ => true | Token.CHAR _ => true
      | Token.LPAREN => true | Token.LBRACK => true | Token.LBRACE => true
      | Token.HASH => true | Token.LET => true
      | _ => false

  fun patAtomStarts t =
    case t of
        Token.ID s => not (isInfix s)
      | Token.UNDERSCORE => true
      | Token.OP => true
      | Token.INT _ => true | Token.WORD _ => true | Token.REAL _ => true
      | Token.STRING _ => true | Token.CHAR _ => true
      | Token.LPAREN => true | Token.LBRACK => true | Token.LBRACE => true
      | _ => false

  fun startsDec t =
    case t of
        Token.VAL => true | Token.FUN => true | Token.TYPE => true
      | Token.DATATYPE => true | Token.EXCEPTION => true | Token.OPEN => true
      | Token.LOCAL => true | Token.INFIX => true | Token.INFIXR => true
      | Token.NONFIX => true | Token.STRUCTURE => true | Token.SIGNATURE => true
      | Token.FUNCTOR => true
      | _ => false

  fun skipSemis () =
    case peek () of Token.SEMICOLON => (adv (); skipSemis ()) | _ => ()

  (* ---- the recursive grammar ---- *)

  fun exp () =
    let val lo = curLo () in
    case peek () of
        Token.FN => (adv (); mk lo (EFn (parseMatch ())))
      | Token.CASE =>
          (adv ();
           let val e = exp ()
           in expect Token.OF; mk lo (ECase (e, parseMatch ())) end)
      | Token.IF =>
          (adv ();
           let val c = exp ()
           in expect Token.THEN;
              let val t = exp ()
              in expect Token.ELSE; mk lo (EIf (c, t, exp ())) end
           end)
      | Token.WHILE =>
          (adv ();
           let val c = exp ()
           in expect Token.DO; mk lo (EWhile (c, exp ())) end)
      | Token.RAISE => (adv (); mk lo (ERaise (exp ())))
      | _ => expHandle ()
    end

  and expHandle () =
    let val e = expOr ()
    in case peek () of
           Token.HANDLE => (adv (); mk (loOfW e) (EHandle (e, parseMatch ())))
         | _ => e
    end

  and expOr () =
    let val e = expAnd ()
    in case peek () of
           Token.ORELSE => (adv (); mk (loOfW e) (EOrelse (e, exp ())))
         | _ => e
    end

  and expAnd () =
    let val e = expTyped ()
    in case peek () of
           Token.ANDALSO => (adv (); mk (loOfW e) (EAndalso (e, exp ())))
         | _ => e
    end

  and expTyped () =
    let val e = infexp ()
    in case peek () of
           Token.COLON => (adv (); mk (loOfW e) (ETyped (e, ty ())))
         | _ => e
    end

  and infexp () =
    let
      fun climb minPrec =
        let
          fun loop left =
            case peekInfixOp () of
                SOME (opid, (prec, assoc)) =>
                  if prec < minPrec then left
                  else
                    (adv ();
                     let
                       val nextMin = case assoc of LeftA => prec + 1 | RightA => prec
                       val right = climb nextMin
                     in loop (mk (loOfW left) (EInfix (opid, left, right))) end)
              | NONE => left
        in loop (appexp ()) end
    in climb 0 end

  and appexp () =
    let
      fun loop e =
        if atexpStarts (peek ())
        then let val x = atexp () in loop (mk (loOfW e) (EApp (e, x))) end
        else e
    in loop (atexp ()) end

  and atexp () =
    let val lo = curLo () in
    case peek () of
        Token.INT s => (adv (); mk lo (ELit (LInt s)))
      | Token.WORD s => (adv (); mk lo (ELit (LWord s)))
      | Token.REAL s => (adv (); mk lo (ELit (LReal s)))
      | Token.STRING s => (adv (); mk lo (ELit (LString s)))
      | Token.CHAR s => (adv (); mk lo (ELit (LChar s)))
      | Token.ID s => (adv (); mk lo (EVar s))
      | Token.OP =>
          (adv ();
           case peek () of
               Token.ID s => (adv (); mk lo (EVar s))
             | Token.EQUALS => (adv (); mk lo (EVar "="))
             | t => raise Parse ("expected identifier after op, got "
                                 ^ Token.toString t))
      | Token.HASH =>
          (adv ();
           case peek () of
               Token.ID s => (adv (); mk lo (ESelector s))
             | Token.INT s => (adv (); mk lo (ESelector s))
             | t => raise Parse ("expected label after #, got "
                                 ^ Token.toString t))
      | Token.LET => parseLet ()
      | Token.LPAREN => parseParenExp ()
      | Token.LBRACK => parseListExp ()
      | Token.LBRACE => parseRecordExp ()
      | t => raise Parse ("expected expression, got " ^ Token.toString t)
    end

  and parseLet () =
    let val lo = curLo () in
    (expect Token.LET;
     let val ds = parseDecs (fn t => t = Token.IN)
     in expect Token.IN;
        let
          fun seqRest acc =
            case peek () of
                Token.SEMICOLON => (adv (); seqRest (exp () :: acc))
              | _ => List.rev acc
          val es = seqRest [exp ()]
          val body = case es of [e] => e | _ => (ESeq es, spanList es)
        in expect Token.END; mk lo (ELet (ds, body)) end
     end)
    end

  and parseParenExp () =
    let val lo = curLo () in
    (expect Token.LPAREN;
     case peek () of
         Token.RPAREN => (adv (); mk lo (ETuple []))
       | _ =>
           let val e1 = exp ()
           in case peek () of
                  Token.RPAREN => (adv (); e1)
                | Token.COMMA =>
                    let
                      fun rest acc =
                        case peek () of
                            Token.COMMA => (adv (); rest (exp () :: acc))
                          | _ => List.rev acc
                      val es = rest [e1]
                    in expect Token.RPAREN; mk lo (ETuple es) end
                | Token.SEMICOLON =>
                    let
                      fun rest acc =
                        case peek () of
                            Token.SEMICOLON => (adv (); rest (exp () :: acc))
                          | _ => List.rev acc
                      val es = rest [e1]
                    in expect Token.RPAREN; mk lo (ESeq es) end
                | t => raise Parse ("expected ) , or ; got " ^ Token.toString t)
           end)
    end

  and parseListExp () =
    let val lo = curLo () in
    (expect Token.LBRACK;
     case peek () of
         Token.RBRACK => (adv (); mk lo (EList []))
       | _ =>
           let
             fun rest acc =
               case peek () of
                   Token.COMMA => (adv (); rest (exp () :: acc))
                 | _ => List.rev acc
             val es = rest [exp ()]
           in expect Token.RBRACK; mk lo (EList es) end)
    end

  and parseRecordExp () =
    let val lo = curLo () in
    (expect Token.LBRACE;
     case peek () of
         Token.RBRACE => (adv (); mk lo (ERecord []))
       | _ =>
           let
             fun field () =
               let val lab = parseLabel ()
               in expect Token.EQUALS; (lab, exp ()) end
             fun rest acc =
               case peek () of
                   Token.COMMA => (adv (); rest (field () :: acc))
                 | _ => List.rev acc
             val fs = rest [field ()]
           in expect Token.RBRACE; mk lo (ERecord fs) end)
    end

  and parseLabel () =
    case peek () of
        Token.ID s => (adv (); s)
      | Token.INT s => (adv (); s)
      | t => raise Parse ("expected label, got " ^ Token.toString t)

  and parseMatch () =
    let
      fun arm () =
        let val p = pat ()
        in expect Token.DARROW; (p, exp ()) end
      fun loop acc =
        let val a = arm ()
        in case peek () of Token.BAR => (adv (); loop (a :: acc))
                         | _ => List.rev (a :: acc)
        end
    in loop [] end

  (* ---- patterns ---- *)

  and pat () =
    let val p = patInfix ()
    in case peek () of
           Token.COLON => (adv (); mk (loOfW p) (PTyped (p, ty ())))
         | Token.AS =>
             (case patNode p of
                  PVar id => (adv (); mk (loOfW p) (PAs (id, pat ())))
                | _ => raise Parse "as requires a variable")
         | _ => p
    end

  and patInfix () =
    let
      fun climb minPrec =
        let
          fun loop left =
            case peek () of
                Token.ID s =>
                  (case lookupFix s of
                       SOME (prec, assoc) =>
                         if prec < minPrec then left
                         else
                           (adv ();
                            let
                              val nextMin =
                                case assoc of LeftA => prec + 1 | RightA => prec
                              val right = climb nextMin
                            in loop (mk (loOfW left) (PInfix (s, left, right))) end)
                     | NONE => left)
              | _ => left
        in loop (patApp ()) end
    in climb 0 end

  and patApp () =
    let val p = patAtom ()
    in case patNode p of
           PVar id =>
             if patAtomStarts (peek ())
             then let val a = patAtom () in mk (loOfW p) (PCon (id, a)) end
             else p
         | _ => p
    end

  and patAtom () =
    let val lo = curLo () in
    case peek () of
        Token.UNDERSCORE => (adv (); mk lo PWild)
      | Token.ID s => (adv (); mk lo (PVar s))
      | Token.OP =>
          (adv ();
           case peek () of
               Token.ID s => (adv (); mk lo (PVar s))
             | Token.EQUALS => (adv (); mk lo (PVar "="))
             | t => raise Parse ("expected id after op, got " ^ Token.toString t))
      | Token.INT s => (adv (); mk lo (PLit (LInt s)))
      | Token.WORD s => (adv (); mk lo (PLit (LWord s)))
      | Token.REAL s => (adv (); mk lo (PLit (LReal s)))
      | Token.STRING s => (adv (); mk lo (PLit (LString s)))
      | Token.CHAR s => (adv (); mk lo (PLit (LChar s)))
      | Token.LPAREN => parseParenPat ()
      | Token.LBRACK => parseListPat ()
      | Token.LBRACE => parseRecordPat ()
      | t => raise Parse ("expected pattern, got " ^ Token.toString t)
    end

  and parseParenPat () =
    let val lo = curLo () in
    (expect Token.LPAREN;
     case peek () of
         Token.RPAREN => (adv (); mk lo (PTuple []))
       | _ =>
           let val p1 = pat ()
           in case peek () of
                  Token.RPAREN => (adv (); p1)
                | Token.COMMA =>
                    let
                      fun rest acc =
                        case peek () of
                            Token.COMMA => (adv (); rest (pat () :: acc))
                          | _ => List.rev acc
                      val ps = rest [p1]
                    in expect Token.RPAREN; mk lo (PTuple ps) end
                | t => raise Parse ("expected ) or , in pattern, got "
                                    ^ Token.toString t)
           end)
    end

  and parseListPat () =
    let val lo = curLo () in
    (expect Token.LBRACK;
     case peek () of
         Token.RBRACK => (adv (); mk lo (PList []))
       | _ =>
           let
             fun rest acc =
               case peek () of
                   Token.COMMA => (adv (); rest (pat () :: acc))
                 | _ => List.rev acc
             val ps = rest [pat ()]
           in expect Token.RBRACK; mk lo (PList ps) end)
    end

  and parseRecordPat () =
    let val lo = curLo () in
    (expect Token.LBRACE;
     let
       fun finish (acc, flex) =
         (expect Token.RBRACE; mk lo (PRecord (List.rev acc, flex)))
       fun fields (acc, flex) =
         case peek () of
             Token.RBRACE => finish (acc, flex)
           | Token.DOTDOTDOT => (adv (); finish (acc, true))
           | _ =>
               let
                 val lab = parseLabel ()
                 val p = case peek () of
                             Token.EQUALS => (adv (); pat ())
                           | _ => (PVar lab, spanAt (!posRef - 1))
               in case peek () of
                      Token.COMMA => (adv (); fields ((lab, p) :: acc, flex))
                    | _ => finish ((lab, p) :: acc, flex)
               end
     in fields ([], false) end)
    end

  (* ---- types (unpositioned) ---- *)

  and ty () = tyArrow ()

  and tyArrow () =
    let val t = tyTuple ()
    in case peek () of Token.ARROW => (adv (); TyArrow (t, ty ())) | _ => t end

  and tyTuple () =
    let
      val t = tyApp ()
      fun loop acc =
        case peek () of
            Token.ID "*" => (adv (); loop (tyApp () :: acc))
          | _ => List.rev acc
      val rest = loop [t]
    in case rest of [single] => single | many => TyTuple many end

  and tyApp () =
    let
      fun loop t =
        case peek () of
            Token.ID s =>
              if s <> "*" then (adv (); loop (TyCon ([t], s))) else t
          | _ => t
    in loop (tyAtom ()) end

  and tyAtom () =
    case peek () of
        Token.TYVAR s => (adv (); TyVar s)
      | Token.ID s => (adv (); TyCon ([], s))
      | Token.LBRACE => parseRecordTy ()
      | Token.LPAREN =>
          (adv ();
           let val t = ty ()
           in case peek () of
                  Token.RPAREN => (adv (); t)
                | Token.COMMA =>
                    let
                      fun rest acc =
                        case peek () of
                            Token.COMMA => (adv (); rest (ty () :: acc))
                          | _ => List.rev acc
                      val ts = rest [t]
                    in expect Token.RPAREN;
                       case peek () of
                           Token.ID s => (adv (); TyCon (ts, s))
                         | tk => raise Parse ("expected type constructor, got "
                                              ^ Token.toString tk)
                    end
                | tk => raise Parse ("expected ) or , in type, got "
                                     ^ Token.toString tk)
           end)
      | t => raise Parse ("expected type, got " ^ Token.toString t)

  and parseRecordTy () =
    (expect Token.LBRACE;
     case peek () of
         Token.RBRACE => (adv (); TyRecord [])
       | _ =>
           let
             fun field () =
               let val lab = parseLabel ()
               in expect Token.COLON; (lab, ty ()) end
             fun rest acc =
               case peek () of
                   Token.COMMA => (adv (); rest (field () :: acc))
                 | _ => List.rev acc
             val fs = rest [field ()]
           in expect Token.RBRACE; TyRecord fs end)

  and parseTyvarSeq () =
    case peek () of
        Token.TYVAR s => (adv (); [s])
      | Token.LPAREN =>
          (case peekN 1 of
               Token.TYVAR _ =>
                 (adv ();
                  let
                    fun loop acc =
                      case peek () of
                          Token.TYVAR s =>
                            (adv ();
                             case peek () of
                                 Token.COMMA => (adv (); loop (s :: acc))
                               | _ => List.rev (s :: acc))
                        | _ => List.rev acc
                    val tvs = loop []
                  in expect Token.RPAREN; tvs end)
             | _ => [])
      | _ => []

  (* ---- declarations ---- *)

  and parseDecs stop =
    let
      fun loop acc =
        if peek () = Token.EOF orelse stop (peek ()) then List.rev acc
        else
          let val d = parseDec ()
          in skipSemis (); loop (d :: acc) end
    in loop [] end

  and parseDec () =
    case peek () of
        Token.VAL => parseVal ()
      | Token.FUN => parseFun ()
      | Token.TYPE => parseTypeDec ()
      | Token.DATATYPE => parseDatatype ()
      | Token.EXCEPTION => parseException ()
      | Token.OPEN => parseOpen ()
      | Token.LOCAL => parseLocal ()
      | Token.INFIX => parseInfix false
      | Token.INFIXR => parseInfix true
      | Token.NONFIX => parseNonfix ()
      | Token.STRUCTURE => parseStructure ()
      | Token.SIGNATURE => parseSignature ()
      | Token.FUNCTOR => parseFunctor ()
      | t => raise Parse ("expected declaration, got " ^ Token.toString t)

  and parseVal () =
    let val lo = curLo () in
    (expect Token.VAL;
     let
       val tvs = parseTyvarSeq ()
       val isRec = case peek () of Token.REC => (adv (); true) | _ => false
       fun binding () =
         let val p = pat ()
         in expect Token.EQUALS; (p, exp ()) end
       fun loop acc =
         let val b = binding ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in mk lo (DVal (tvs, loop [], isRec)) end)
    end

  and parseFun () =
    let val lo = curLo () in
    (expect Token.FUN;
     let
       val tvs = parseTyvarSeq ()
       fun clause () =
         let
           val name = parseVid ()
           fun args acc =
             if patAtomStarts (peek ()) then args (patAtom () :: acc)
             else List.rev acc
           val ps = args []
           val ret = case peek () of Token.COLON => (adv (); SOME (ty ()))
                                   | _ => NONE
           val _ = expect Token.EQUALS
           val body = exp ()
         in (name, { pats = ps, ret = ret, body = body }) end
       fun clauses (nm, cs) =
         case peek () of
             Token.BAR =>
               (adv ();
                let val (_, c) = clause ()
                in clauses (nm, c :: cs) end)
           | _ => (nm, List.rev cs)
       fun functions acc =
         let
           val (nm, c) = clause ()
           val fdef = clauses (nm, [c])
         in case peek () of
                Token.AND => (adv (); functions (fdef :: acc))
              | _ => List.rev (fdef :: acc)
         end
     in mk lo (DFun (tvs, functions [])) end)
    end

  and parseVid () =
    case peek () of
        Token.ID s => (adv (); s)
      | Token.OP =>
          (adv ();
           case peek () of
               Token.ID s => (adv (); s)
             | Token.EQUALS => (adv (); "=")
             | t => raise Parse ("expected id after op, got " ^ Token.toString t))
      | t => raise Parse ("expected identifier, got " ^ Token.toString t)

  and parseTyConName () =
    case peek () of
        Token.ID s => (adv (); s)
      | t => raise Parse ("expected type constructor name, got "
                          ^ Token.toString t)

  and parseTypeDec () =
    let val lo = curLo () in
    (expect Token.TYPE;
     let
       fun bind () =
         let
           val tvs = parseTyvarSeq ()
           val name = parseTyConName ()
           val _ = expect Token.EQUALS
         in (tvs, name, ty ()) end
       fun loop acc =
         let val b = bind ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in mk lo (DType (loop [])) end)
    end

  and parseDatbinds () =
    let
      fun con () =
        let val c = parseVid ()
        in case peek () of Token.OF => (adv (); (c, SOME (ty ())))
                         | _ => (c, NONE)
        end
      fun cons acc =
        let val c = con ()
        in case peek () of Token.BAR => (adv (); cons (c :: acc))
                         | _ => List.rev (c :: acc)
        end
      fun bind () =
        let
          val tvs = parseTyvarSeq ()
          val name = parseTyConName ()
          val _ = expect Token.EQUALS
        in { tyvars = tvs, name = name, cons = cons [] } end
      fun loop acc =
        let val b = bind ()
        in case peek () of Token.AND => (adv (); loop (b :: acc))
                         | _ => List.rev (b :: acc)
        end
    in loop [] end

  and parseDatatype () =
    let val lo = curLo () in
    (expect Token.DATATYPE;
     let
       val dbs = parseDatbinds ()
       val withs =
         case peek () of
             Token.WITHTYPE =>
               (adv ();
                let
                  fun tb () =
                    let
                      val tvs = parseTyvarSeq ()
                      val nm = parseTyConName ()
                      val _ = expect Token.EQUALS
                    in (tvs, nm, ty ()) end
                  fun loop acc =
                    let val b = tb ()
                    in case peek () of Token.AND => (adv (); loop (b :: acc))
                                     | _ => List.rev (b :: acc)
                    end
                in loop [] end)
           | _ => []
     in mk lo (DDatatype (dbs, withs)) end)
    end

  and parseException () =
    let val lo = curLo () in
    (expect Token.EXCEPTION;
     let
       fun eb () =
         let val c = parseVid ()
         in case peek () of Token.OF => (adv (); (c, SOME (ty ())))
                          | _ => (c, NONE)
         end
       fun loop acc =
         let val b = eb ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in mk lo (DException (loop [])) end)
    end

  and parseOpen () =
    let val lo = curLo () in
    (expect Token.OPEN;
     let
       fun loop acc =
         case peek () of Token.ID s => (adv (); loop (s :: acc))
                       | _ => List.rev acc
       val ids = loop []
     in if null ids then raise Parse "open needs an identifier"
        else mk lo (DOpen ids)
     end)
    end

  and parseLocal () =
    let val lo = curLo () in
    (expect Token.LOCAL;
     let val d1 = parseDecs (fn t => t = Token.IN)
     in expect Token.IN;
        let val d2 = parseDecs (fn t => t = Token.END)
        in expect Token.END; mk lo (DLocal (d1, d2)) end
     end)
    end

  and parseInfix isRight =
    let val lo = curLo () in
    (adv ();
     let
       val prec = case peek () of
                      Token.INT s => (adv (); valOf (Int.fromString s))
                    | _ => 0
       fun loop acc =
         case peek () of
             Token.ID s => (adv (); loop (s :: acc))
           | Token.EQUALS => (adv (); loop ("=" :: acc))
           | _ => List.rev acc
       val ids = loop []
       val assoc = if isRight then RightA else LeftA
       val () = List.app (fn id => addFixity (id, prec, assoc)) ids
     in if isRight then mk lo (DInfixr (prec, ids)) else mk lo (DInfix (prec, ids))
     end)
    end

  and parseNonfix () =
    let val lo = curLo () in
    (expect Token.NONFIX;
     let
       fun loop acc =
         case peek () of
             Token.ID s => (adv (); loop (s :: acc))
           | Token.EQUALS => (adv (); loop ("=" :: acc))
           | _ => List.rev acc
       val ids = loop []
       val () = List.app removeFixity ids
     in mk lo (DNonfix ids) end)
    end

  and parseStructure () =
    let val lo = curLo () in
    (expect Token.STRUCTURE;
     let
       fun bind () =
         let
           val name = parseTyConName ()
           val asc = case peek () of
                         Token.COLON => (adv (); SOME (false, sigexp ()))
                       | Token.COLONGT => (adv (); SOME (true, sigexp ()))
                       | _ => NONE
           val _ = expect Token.EQUALS
           val body = strexp ()
           val body2 = case asc of
                           SOME (opq, se) => StrConstraint (body, se, opq)
                         | NONE => body
         in (name, body2) end
       fun loop acc =
         let val b = bind ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in mk lo (DStructure (loop [])) end)
    end

  and strexp () =
    let
      fun base () =
        case peek () of
            Token.STRUCT =>
              (adv ();
               let val ds = parseDecs (fn t => t = Token.END)
               in expect Token.END; StrStruct ds end)
          | Token.LET =>
              (adv ();
               let val ds = parseDecs (fn t => t = Token.IN)
               in expect Token.IN;
                  let val se = strexp ()
                  in expect Token.END; StrLet (ds, se) end
               end)
          | Token.ID s =>
              (adv ();
               case peek () of
                   Token.LPAREN =>
                     (adv ();
                      let
                        val arg =
                          if startsDec (peek ()) then
                            StrStruct (parseDecs (fn t => t = Token.RPAREN))
                          else strexp ()
                      in expect Token.RPAREN; StrApp (s, arg) end)
                 | _ => StrId s)
          | t => raise Parse ("expected structure expression, got "
                              ^ Token.toString t)
      fun loop se =
        case peek () of
            Token.COLON => (adv (); loop (StrConstraint (se, sigexp (), false)))
          | Token.COLONGT => (adv (); loop (StrConstraint (se, sigexp (), true)))
          | _ => se
    in loop (base ()) end

  and parseSignature () =
    let val lo = curLo () in
    (expect Token.SIGNATURE;
     let
       fun bind () =
         let
           val name = parseTyConName ()
           val _ = expect Token.EQUALS
         in (name, sigexp ()) end
       fun loop acc =
         let val b = bind ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in mk lo (DSignature (loop [])) end)
    end

  and sigexp () =
    let
      fun base () =
        case peek () of
            Token.SIG =>
              (adv ();
               let val sps = parseSpecs ()
               in expect Token.END; SigSig sps end)
          | Token.ID s => (adv (); SigId s)
          | t => raise Parse ("expected signature expression, got "
                              ^ Token.toString t)
      fun loop se =
        case peek () of
            Token.WHERE =>
              (adv (); expect Token.TYPE;
               let
                 val tvs = parseTyvarSeq ()
                 val nm = parseTyConName ()
                 val _ = expect Token.EQUALS
                 val t = ty ()
               in loop (SigWhere (se, [(tvs, nm, t)])) end)
          | _ => se
    in loop (base ()) end

  and parseSpecs () =
    let
      fun loop acc =
        if peek () = Token.EOF orelse peek () = Token.END then List.rev acc
        else let val s = parseSpec () in skipSemis (); loop (s :: acc) end
    in loop [] end

  and parseSpec () =
    let val lo = curLo () in
    case peek () of
        Token.VAL =>
          (adv ();
           let
             fun vb () =
               let val v = parseVid ()
               in expect Token.COLON; (v, ty ()) end
             fun loop acc =
               let val b = vb ()
               in case peek () of Token.AND => (adv (); loop (b :: acc))
                                | _ => List.rev (b :: acc)
               end
           in mk lo (SpecVal (loop [])) end)
      | Token.TYPE =>
          (adv ();
           let
             fun tb () =
               let
                 val tvs = parseTyvarSeq ()
                 val nm = parseTyConName ()
               in case peek () of
                      Token.EQUALS => (adv (); (tvs, nm, SOME (ty ())))
                    | _ => (tvs, nm, NONE)
               end
             fun loop acc =
               let val b = tb ()
               in case peek () of Token.AND => (adv (); loop (b :: acc))
                                | _ => List.rev (b :: acc)
               end
             val bs = loop []
             val allDef = List.all (fn (_, _, x) => Option.isSome x) bs
           in
             if allDef then
               mk lo (SpecTypeDef (List.map (fn (tv, n, x) => (tv, n, valOf x)) bs))
             else mk lo (SpecType (List.map (fn (tv, n, _) => (tv, n)) bs))
           end)
      | Token.EQTYPE =>
          (adv ();
           let
             fun tb () =
               let val tvs = parseTyvarSeq () in (tvs, parseTyConName ()) end
             fun loop acc =
               let val b = tb ()
               in case peek () of Token.AND => (adv (); loop (b :: acc))
                                | _ => List.rev (b :: acc)
               end
           in mk lo (SpecEqtype (loop [])) end)
      | Token.DATATYPE => (adv (); mk lo (SpecDatatype (parseDatbinds ())))
      | Token.EXCEPTION =>
          (adv ();
           let
             fun eb () =
               let val c = parseVid ()
               in case peek () of Token.OF => (adv (); (c, SOME (ty ())))
                                | _ => (c, NONE)
               end
             fun loop acc =
               let val b = eb ()
               in case peek () of Token.AND => (adv (); loop (b :: acc))
                                | _ => List.rev (b :: acc)
               end
           in mk lo (SpecException (loop [])) end)
      | Token.STRUCTURE =>
          (adv ();
           let
             fun sb () =
               let val nm = parseTyConName ()
               in expect Token.COLON; (nm, sigexp ()) end
             fun loop acc =
               let val b = sb ()
               in case peek () of Token.AND => (adv (); loop (b :: acc))
                                | _ => List.rev (b :: acc)
               end
           in mk lo (SpecStructure (loop [])) end)
      | Token.INCLUDE => (adv (); mk lo (SpecInclude (sigexp ())))
      | t => raise Parse ("expected specification, got " ^ Token.toString t)
    end

  and parseFunctor () =
    let val lo = curLo () in
    (expect Token.FUNCTOR;
     let
       fun bind () =
         let
           val name = parseTyConName ()
           val _ = expect Token.LPAREN
           val argName = parseTyConName ()
           val _ = expect Token.COLON
           val argSig = sigexp ()
           val _ = expect Token.RPAREN
           val asc = case peek () of
                         Token.COLON => (adv (); SOME (false, sigexp ()))
                       | Token.COLONGT => (adv (); SOME (true, sigexp ()))
                       | _ => NONE
           val _ = expect Token.EQUALS
           val body = strexp ()
         in { name = name, arg = argName, argSig = argSig,
              ascription = asc, body = body } end
       fun loop acc =
         let val b = bind ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in mk lo (DFunctor (loop [])) end)
    end

  (* ---- entry points ---- *)

  fun reset ptokens =
    (toksRef := Vector.fromList (List.map (fn (t, _) => t) ptokens);
     spansRef := Vector.fromList (List.map (fn (_, sp) => sp) ptokens);
     posRef := 0;
     fixityRef := defaultFixity)

  fun parse ptokens =
    let
      val () = reset ptokens
      val ds = parseDecs (fn _ => false)
    in case peek () of
           Token.EOF => ds
         | t => raise Parse ("trailing input: " ^ Token.toString t)
    end

  fun parseString s = parse (Lexer.tokenize s)

  fun parseExp s =
    let
      val () = reset (Lexer.tokenize s)
      val e = exp ()
    in case peek () of
           Token.EOF => e
         | t => raise Parse ("trailing input: " ^ Token.toString t)
    end
end
