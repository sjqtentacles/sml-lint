(* ppast.sml - see ppast.sig.

   Spans are IGNORED here: every wrapped node (exp/pat/dec/spec) is inspected
   via its node projection, so the rendered text is identical to the
   position-free v1 output and the parse->pp round trip stays idempotent. *)

structure PpAst :> PPAST =
struct
  open Ast

  (* ---- literal / identifier helpers ---- *)

  fun pad3 n =
    let val s = Int.toString n
    in case String.size s of 1 => "00" ^ s | 2 => "0" ^ s | _ => s end

  fun escChar c =
    let val n = Char.ord c in
      if c = #"\"" then "\\\""
      else if c = #"\\" then "\\\\"
      else if c = #"\n" then "\\n"
      else if c = #"\t" then "\\t"
      else if n = 13 then "\\r"
      else if n >= 32 andalso n <= 126 then String.str c
      else "\\" ^ pad3 n
    end

  fun escStr s = String.concat (List.map escChar (String.explode s))

  fun isSymStart s =
    s <> "" andalso Char.contains "!%&$#+-/:<=>?@\\~`^|*" (String.sub (s, 0))

  fun infixName s =
    isSymStart s orelse List.exists (fn x => x = s) ["div", "mod", "o", "before"]

  (* `op`-prefix an infix identifier used in value/binding position. *)
  fun ppVar s = if infixName s then "op " ^ s else s

  fun tvseq [] = ""
    | tvseq [a] = a ^ " "
    | tvseq xs = "(" ^ String.concatWith ", " xs ^ ") "

  fun fmtLit l =
    case l of
        LInt s => s
      | LWord s => s
      | LReal s => s
      | LChar c => "#\"" ^ escStr c ^ "\""
      | LString s => "\"" ^ escStr s ^ "\""

  (* atomicity tests operate on the bare node (span ignored) *)
  fun isAtomExp e =
    case e of
        ELit _ => true | EVar _ => true | ESelector _ => true
      | ETuple _ => true | EList _ => true | ERecord _ => true | ESeq _ => true
      | ELet _ => true | EInfix _ => true | ETyped _ => true
      | EAndalso _ => true | EOrelse _ => true
      | _ => false

  fun isAtomPat p =
    case p of
        PWild => true | PVar _ => true | PLit _ => true
      | PTuple _ => true | PList _ => true | PRecord _ => true
      | PInfix _ => true | PTyped _ => true
      | _ => false

  (* ---- the mutually-recursive printers ---- *)

  fun tyP t =
    case t of
        TyVar s => s
      | TyCon ([], c) => c
      | TyCon ([t1], c) => tyAtomP t1 ^ " " ^ c
      | TyCon (ts, c) => "(" ^ String.concatWith ", " (List.map tyP ts) ^ ") " ^ c
      | TyTuple ts => String.concatWith " * " (List.map tyStarP ts)
      | TyArrow (a, b) => tyArrLP a ^ " -> " ^ tyP b
      | TyRecord fs =>
          "{" ^ String.concatWith ", "
                  (List.map (fn (l, t) => l ^ " : " ^ tyP t) fs) ^ "}"
  and tyAtomP t =
    case t of TyArrow _ => "(" ^ tyP t ^ ")"
            | TyTuple _ => "(" ^ tyP t ^ ")"
            | _ => tyP t
  and tyStarP t =
    case t of TyArrow _ => "(" ^ tyP t ^ ")"
            | TyTuple _ => "(" ^ tyP t ^ ")"
            | _ => tyP t
  and tyArrLP t =
    case t of TyArrow _ => "(" ^ tyP t ^ ")" | _ => tyP t

  and patP pw =
    case patNode pw of
        PWild => "_"
      | PVar s => ppVar s
      | PLit l => fmtLit l
      | PTuple [] => "()"
      | PTuple ps => "(" ^ String.concatWith ", " (List.map patP ps) ^ ")"
      | PList ps => "[" ^ String.concatWith ", " (List.map patP ps) ^ "]"
      | PRecord (fs, flex) =>
          "{" ^ String.concatWith ", "
                  (List.map (fn (l, p) => l ^ " = " ^ patP p) fs
                   @ (if flex then ["..."] else [])) ^ "}"
      | PCon (c, p) => ppVar c ^ " " ^ patAtomP p
      | PInfix (oper, l, r) => "(" ^ patP l ^ " " ^ oper ^ " " ^ patP r ^ ")"
      | PTyped (p, t) => "(" ^ patP p ^ " : " ^ tyP t ^ ")"
      | PAs (id, p) => id ^ " as " ^ patAtomP p
  and patAtomP pw = if isAtomPat (patNode pw) then patP pw else "(" ^ patP pw ^ ")"

  and expP ind ew =
    case expNode ew of
        ELit l => fmtLit l
      | EVar s => ppVar s
      | ESelector s => "#" ^ s
      | ETuple [] => "()"
      | ETuple es => "(" ^ String.concatWith ", " (List.map (expP ind) es) ^ ")"
      | EList es => "[" ^ String.concatWith ", " (List.map (expP ind) es) ^ "]"
      | ERecord fs =>
          "{" ^ String.concatWith ", "
                  (List.map (fn (l, e) => l ^ " = " ^ expP ind e) fs) ^ "}"
      | ESeq es => "(" ^ String.concatWith "; " (List.map (expP ind) es) ^ ")"
      | EInfix (oper, l, r) =>
          "(" ^ expP ind l ^ " " ^ oper ^ " " ^ expP ind r ^ ")"
      | ETyped (e, t) => "(" ^ expP ind e ^ " : " ^ tyP t ^ ")"
      | EAndalso (l, r) => "(" ^ expP ind l ^ " andalso " ^ expP ind r ^ ")"
      | EOrelse (l, r) => "(" ^ expP ind l ^ " orelse " ^ expP ind r ^ ")"
      | EApp _ => appP ind ew
      | ERaise e => "raise " ^ barP ind e
      | EIf (c, t, f) =>
          "if " ^ barP ind c ^ " then " ^ barP ind t ^ " else " ^ barP ind f
      | EWhile (c, b) => "while " ^ barP ind c ^ " do " ^ barP ind b
      | ECase (e, ms) => "case " ^ expP ind e ^ " of " ^ matchP ind ms
      | EFn ms => "fn " ^ matchP ind ms
      | EHandle (e, ms) => protP ind e ^ " handle " ^ matchP ind ms
      | ELet (ds, body) =>
          "let\n" ^ decsP (ind ^ "  ") ds ^ "\n" ^ ind ^ "in\n"
          ^ ind ^ "  " ^ expP (ind ^ "  ") body ^ "\n" ^ ind ^ "end"
  and atomP ind ew = if isAtomExp (expNode ew) then expP ind ew
                     else "(" ^ expP ind ew ^ ")"
  and barP ind ew =
    case expNode ew of
        ECase _ => "(" ^ expP ind ew ^ ")"
      | EFn _ => "(" ^ expP ind ew ^ ")"
      | EHandle _ => "(" ^ expP ind ew ^ ")"
      | _ => expP ind ew
  and protP ind ew =
    case expNode ew of
        EIf _ => "(" ^ expP ind ew ^ ")"
      | EWhile _ => "(" ^ expP ind ew ^ ")"
      | ECase _ => "(" ^ expP ind ew ^ ")"
      | EFn _ => "(" ^ expP ind ew ^ ")"
      | ERaise _ => "(" ^ expP ind ew ^ ")"
      | EHandle _ => "(" ^ expP ind ew ^ ")"
      | _ => expP ind ew
  and appP ind ew =
    case expNode ew of
        EApp (f, x) => appP ind f ^ " " ^ atomP ind x
      | _ => atomP ind ew
  and matchP ind ms =
    String.concatWith " | "
      (List.map (fn (p, e) => patP p ^ " => " ^ barP ind e) ms)

  and decsP ind ds = String.concatWith "\n" (List.map (decP ind) ds)
  and decP ind dw =
    case decNode dw of
        DVal (tvs, binds, isRec) =>
          ind ^ "val " ^ (if isRec then "rec " else "") ^ tvseq tvs
          ^ String.concatWith ("\n" ^ ind ^ "and ")
              (List.map (fn (p, e) => patP p ^ " = " ^ expP ind e) binds)
      | DFun (tvs, funs) =>
          ind ^ "fun " ^ tvseq tvs
          ^ String.concatWith ("\n" ^ ind ^ "and ") (List.map (funP ind) funs)
      | DType binds =>
          ind ^ "type " ^ String.concatWith " and "
              (List.map (fn (tvs, nm, t) => tvseq tvs ^ nm ^ " = " ^ tyP t) binds)
      | DDatatype (dbs, withs) =>
          ind ^ "datatype " ^ String.concatWith " and " (List.map datbindP dbs)
          ^ (if null withs then ""
             else "\n" ^ ind ^ "withtype " ^ String.concatWith " and "
                    (List.map (fn (tvs, nm, t) => tvseq tvs ^ nm ^ " = " ^ tyP t)
                              withs))
      | DException ebs =>
          ind ^ "exception " ^ String.concatWith " and " (List.map exbindP ebs)
      | DOpen ids => ind ^ "open " ^ String.concatWith " " ids
      | DLocal (d1, d2) =>
          ind ^ "local\n" ^ decsP (ind ^ "  ") d1 ^ "\n" ^ ind ^ "in\n"
          ^ decsP (ind ^ "  ") d2 ^ "\n" ^ ind ^ "end"
      | DInfix (p, ids) =>
          ind ^ "infix " ^ Int.toString p ^ " " ^ String.concatWith " " ids
      | DInfixr (p, ids) =>
          ind ^ "infixr " ^ Int.toString p ^ " " ^ String.concatWith " " ids
      | DNonfix ids => ind ^ "nonfix " ^ String.concatWith " " ids
      | DStructure binds =>
          String.concatWith "\n"
            (List.map (fn (nm, se) =>
                ind ^ "structure " ^ nm ^ " = " ^ strexpP ind se) binds)
      | DSignature binds =>
          String.concatWith "\n"
            (List.map (fn (nm, se) =>
                ind ^ "signature " ^ nm ^ " = " ^ sigexpP ind se) binds)
      | DFunctor binds => String.concatWith "\n" (List.map (fctbindP ind) binds)

  and funP ind (name, clauses) =
    String.concatWith " | "
      (List.map (fn {pats, ret, body} =>
          ppVar name ^ " " ^ String.concatWith " " (List.map patAtomP pats)
          ^ (case ret of SOME t => " : " ^ tyP t | NONE => "")
          ^ " = " ^ expP ind body) clauses)

  and datbindP {tyvars, name, cons} =
    tvseq tyvars ^ name ^ " = "
    ^ String.concatWith " | "
        (List.map (fn (c, NONE) => ppVar c
                    | (c, SOME t) => ppVar c ^ " of " ^ tyP t) cons)

  and exbindP (c, NONE) = ppVar c
    | exbindP (c, SOME t) = ppVar c ^ " of " ^ tyP t

  and fctbindP ind {name, arg, argSig, ascription, body} =
    ind ^ "functor " ^ name ^ " (" ^ arg ^ " : " ^ sigexpP ind argSig ^ ")"
    ^ (case ascription of
           SOME (opq, se) => (if opq then " :> " else " : ") ^ sigexpP ind se
         | NONE => "")
    ^ " = " ^ strexpP ind body

  and strexpP ind se =
    case se of
        StrStruct [] => "struct end"
      | StrStruct ds => "struct\n" ^ decsP (ind ^ "  ") ds ^ "\n" ^ ind ^ "end"
      | StrId s => s
      | StrApp (f, se) => f ^ " (" ^ strexpP ind se ^ ")"
      | StrLet (ds, se) =>
          "let\n" ^ decsP (ind ^ "  ") ds ^ "\n" ^ ind ^ "in\n"
          ^ ind ^ "  " ^ strexpP (ind ^ "  ") se ^ "\n" ^ ind ^ "end"
      | StrConstraint (se, sg, opq) =>
          strexpP ind se ^ (if opq then " :> " else " : ") ^ sigexpP ind sg

  and sigexpP ind sg =
    case sg of
        SigSig [] => "sig end"
      | SigSig sps => "sig\n" ^ specsP (ind ^ "  ") sps ^ "\n" ^ ind ^ "end"
      | SigId s => s
      | SigWhere (se, binds) =>
          sigexpP ind se
          ^ String.concat (List.map (fn (tvs, nm, t) =>
                " where type " ^ tvseq tvs ^ nm ^ " = " ^ tyP t) binds)

  and specsP ind sps = String.concatWith "\n" (List.map (specP ind) sps)
  and specP ind spw =
    case specNode spw of
        SpecVal binds =>
          ind ^ "val " ^ String.concatWith " and "
            (List.map (fn (v, t) => v ^ " : " ^ tyP t) binds)
      | SpecType binds =>
          ind ^ "type " ^ String.concatWith " and "
            (List.map (fn (tvs, nm) => tvseq tvs ^ nm) binds)
      | SpecEqtype binds =>
          ind ^ "eqtype " ^ String.concatWith " and "
            (List.map (fn (tvs, nm) => tvseq tvs ^ nm) binds)
      | SpecTypeDef binds =>
          ind ^ "type " ^ String.concatWith " and "
            (List.map (fn (tvs, nm, t) => tvseq tvs ^ nm ^ " = " ^ tyP t) binds)
      | SpecDatatype dbs =>
          ind ^ "datatype " ^ String.concatWith " and " (List.map datbindP dbs)
      | SpecException ebs =>
          ind ^ "exception " ^ String.concatWith " and " (List.map exbindP ebs)
      | SpecStructure binds =>
          String.concatWith "\n"
            (List.map (fn (nm, se) =>
                ind ^ "structure " ^ nm ^ " : " ^ sigexpP ind se) binds)
      | SpecInclude se => ind ^ "include " ^ sigexpP ind se

  (* ---- public entry points ---- *)

  fun ppTy t = tyP t
  fun ppPat p = patP p
  fun ppExp e = expP "" e
  fun ppDec d = decP "" d
  fun ppProgram ds = decsP "" ds
end
