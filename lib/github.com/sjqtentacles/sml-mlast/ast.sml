(* ast.sml - see ast.sig. Transparent ascription keeps constructors visible.

   v2: exp/pat/dec/spec carry source spans via the `<node> * span` wrapping. *)

structure Ast : AST =
struct
  type tyvar = string
  type ident = string

  type pos = Pos.pos
  type span = Pos.span

  datatype lit =
      LInt of string
    | LWord of string
    | LReal of string
    | LChar of string
    | LString of string

  datatype ty =
      TyVar of tyvar
    | TyCon of ty list * ident
    | TyTuple of ty list
    | TyArrow of ty * ty
    | TyRecord of (string * ty) list

  datatype patnode =
      PWild
    | PVar of ident
    | PLit of lit
    | PTuple of pat list
    | PList of pat list
    | PRecord of (string * pat) list * bool
    | PCon of ident * pat
    | PInfix of ident * pat * pat
    | PTyped of pat * ty
    | PAs of ident * pat
  withtype pat = patnode * span

  datatype expnode =
      ELit of lit
    | EVar of ident
    | ETuple of exp list
    | EList of exp list
    | ERecord of (string * exp) list
    | ESelector of string
    | ESeq of exp list
    | EApp of exp * exp
    | EInfix of ident * exp * exp
    | ETyped of exp * ty
    | EAndalso of exp * exp
    | EOrelse of exp * exp
    | EHandle of exp * (pat * exp) list
    | ERaise of exp
    | EIf of exp * exp * exp
    | EWhile of exp * exp
    | ECase of exp * (pat * exp) list
    | EFn of (pat * exp) list
    | ELet of dec list * exp

  and decnode =
      DVal of tyvar list * (pat * exp) list * bool
    | DFun of tyvar list
              * (ident * { pats : pat list, ret : ty option, body : exp } list) list
    | DType of (tyvar list * ident * ty) list
    | DDatatype of
        { tyvars : tyvar list, name : ident, cons : (ident * ty option) list } list
        * (tyvar list * ident * ty) list
    | DException of (ident * ty option) list
    | DOpen of ident list
    | DLocal of dec list * dec list
    | DInfix of int * ident list
    | DInfixr of int * ident list
    | DNonfix of ident list
    | DStructure of (ident * strexp) list
    | DSignature of (ident * sigexp) list
    | DFunctor of
        { name : ident, arg : ident, argSig : sigexp,
          ascription : (bool * sigexp) option, body : strexp } list

  and strexp =
      StrStruct of dec list
    | StrId of ident
    | StrApp of ident * strexp
    | StrLet of dec list * strexp
    | StrConstraint of strexp * sigexp * bool

  and sigexp =
      SigSig of spec list
    | SigId of ident
    | SigWhere of sigexp * (tyvar list * ident * ty) list

  and specnode =
      SpecVal of (ident * ty) list
    | SpecType of (tyvar list * ident) list
    | SpecEqtype of (tyvar list * ident) list
    | SpecTypeDef of (tyvar list * ident * ty) list
    | SpecDatatype of
        { tyvars : tyvar list, name : ident, cons : (ident * ty option) list } list
    | SpecException of (ident * ty option) list
    | SpecStructure of (ident * sigexp) list
    | SpecInclude of sigexp

  withtype exp = expnode * span
  and dec = decnode * span
  and spec = specnode * span

  type fclause = { pats : pat list, ret : ty option, body : exp }
  type datbind = { tyvars : tyvar list, name : ident,
                   cons : (ident * ty option) list }
  type fctbind = { name : ident, arg : ident, argSig : sigexp,
                   ascription : (bool * sigexp) option, body : strexp }

  type program = dec list

  (* ---- span accessors ---- *)

  fun expSpan  ((_, s) : exp)  = s
  fun patSpan  ((_, s) : pat)  = s
  fun decSpan  ((_, s) : dec)  = s
  fun specSpan ((_, s) : spec) = s

  fun expNode  ((n, _) : exp)  = n
  fun patNode  ((n, _) : pat)  = n
  fun decNode  ((n, _) : dec)  = n
  fun specNode ((n, _) : spec) = n

  (* ---- span erasure for span-insensitive equality ---- *)

  val z = Pos.zero

  fun eraseExp (n, _) = (eraseExpNode n, z)
  and eraseExpNode n =
    case n of
        ELit l => ELit l
      | EVar s => EVar s
      | ETuple es => ETuple (List.map eraseExp es)
      | EList es => EList (List.map eraseExp es)
      | ERecord fs => ERecord (List.map (fn (l, e) => (l, eraseExp e)) fs)
      | ESelector s => ESelector s
      | ESeq es => ESeq (List.map eraseExp es)
      | EApp (a, b) => EApp (eraseExp a, eraseExp b)
      | EInfix (i, a, b) => EInfix (i, eraseExp a, eraseExp b)
      | ETyped (e, t) => ETyped (eraseExp e, t)
      | EAndalso (a, b) => EAndalso (eraseExp a, eraseExp b)
      | EOrelse (a, b) => EOrelse (eraseExp a, eraseExp b)
      | EHandle (e, ms) => EHandle (eraseExp e, List.map eraseArm ms)
      | ERaise e => ERaise (eraseExp e)
      | EIf (a, b, c) => EIf (eraseExp a, eraseExp b, eraseExp c)
      | EWhile (a, b) => EWhile (eraseExp a, eraseExp b)
      | ECase (e, ms) => ECase (eraseExp e, List.map eraseArm ms)
      | EFn ms => EFn (List.map eraseArm ms)
      | ELet (ds, e) => ELet (List.map eraseDec ds, eraseExp e)

  and eraseArm (p, e) = (erasePat p, eraseExp e)

  and erasePat (n, _) = (erasePatNode n, z)
  and erasePatNode n =
    case n of
        PWild => PWild
      | PVar s => PVar s
      | PLit l => PLit l
      | PTuple ps => PTuple (List.map erasePat ps)
      | PList ps => PList (List.map erasePat ps)
      | PRecord (fs, flex) =>
          PRecord (List.map (fn (l, p) => (l, erasePat p)) fs, flex)
      | PCon (c, p) => PCon (c, erasePat p)
      | PInfix (i, a, b) => PInfix (i, erasePat a, erasePat b)
      | PTyped (p, t) => PTyped (erasePat p, t)
      | PAs (id, p) => PAs (id, erasePat p)

  and eraseDec (n, _) = (eraseDecNode n, z)
  and eraseDecNode n =
    case n of
        DVal (tvs, binds, r) =>
          DVal (tvs, List.map (fn (p, e) => (erasePat p, eraseExp e)) binds, r)
      | DFun (tvs, funs) =>
          DFun (tvs, List.map (fn (nm, cls) => (nm, List.map eraseClause cls)) funs)
      | DType bs => DType bs
      | DDatatype (dbs, withs) => DDatatype (dbs, withs)
      | DException ebs => DException ebs
      | DOpen ids => DOpen ids
      | DLocal (d1, d2) => DLocal (List.map eraseDec d1, List.map eraseDec d2)
      | DInfix x => DInfix x
      | DInfixr x => DInfixr x
      | DNonfix x => DNonfix x
      | DStructure binds =>
          DStructure (List.map (fn (nm, se) => (nm, eraseStrexp se)) binds)
      | DSignature binds =>
          DSignature (List.map (fn (nm, sg) => (nm, eraseSigexp sg)) binds)
      | DFunctor binds => DFunctor (List.map eraseFctbind binds)

  and eraseClause { pats, ret, body } =
    { pats = List.map erasePat pats, ret = ret, body = eraseExp body }

  and eraseStrexp se =
    case se of
        StrStruct ds => StrStruct (List.map eraseDec ds)
      | StrId s => StrId s
      | StrApp (f, se') => StrApp (f, eraseStrexp se')
      | StrLet (ds, se') => StrLet (List.map eraseDec ds, eraseStrexp se')
      | StrConstraint (se', sg, opq) =>
          StrConstraint (eraseStrexp se', eraseSigexp sg, opq)

  and eraseSigexp sg =
    case sg of
        SigSig sps => SigSig (List.map eraseSpec sps)
      | SigId s => SigId s
      | SigWhere (sg', bs) => SigWhere (eraseSigexp sg', bs)

  and eraseSpec (n, _) = (eraseSpecNode n, z)
  and eraseSpecNode n =
    case n of
        SpecVal bs => SpecVal bs
      | SpecType bs => SpecType bs
      | SpecEqtype bs => SpecEqtype bs
      | SpecTypeDef bs => SpecTypeDef bs
      | SpecDatatype dbs => SpecDatatype dbs
      | SpecException ebs => SpecException ebs
      | SpecStructure binds =>
          SpecStructure (List.map (fn (nm, sg) => (nm, eraseSigexp sg)) binds)
      | SpecInclude sg => SpecInclude (eraseSigexp sg)

  and eraseFctbind { name, arg, argSig, ascription, body } =
    { name = name, arg = arg, argSig = eraseSigexp argSig,
      ascription = Option.map (fn (b, sg) => (b, eraseSigexp sg)) ascription,
      body = eraseStrexp body }

  fun eraseProgram ds = List.map eraseDec ds
end
