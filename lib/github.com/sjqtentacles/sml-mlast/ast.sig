(* ast.sig

   Abstract syntax for an SML'97 (Core + Modules) subset: types, patterns,
   expressions, declarations, structures, signatures and specs. The tree is
   intentionally "surface" - redundant parentheses are not represented (the
   parser drops grouping parens; the pretty-printer re-inserts the minimal,
   deterministic set), which is what makes parse->pp idempotent.

   Identifiers are stored with their full (possibly qualified) spelling, e.g.
   "List.map". Numeric/char/string literals carry a normalised payload.

   SOURCE POSITIONS (v2, breaking): the recursive node types that tooling needs
   - expressions, patterns, declarations and specs - carry a source `span`.
   The encoding wraps each as `<node> * span`: e.g. an `exp` is an `expnode`
   paired with its span. This keeps the wrapping uniform so consumers strip a
   span in exactly one mechanical place. Pretty-printing IGNORES spans, so the
   round-trip / idempotence property is unchanged. `ty`, `strexp` and `sigexp`
   are not (yet) positioned.

   Spans are 0-based and end-exclusive (see Pos). The datatypes are exposed
   transparently. *)

signature AST =
sig
  type tyvar = string
  type ident = string

  type pos = Pos.pos      (* { line : int, col : int }, 0-based *)
  type span = Pos.span    (* { lo : pos, hi : pos }, end-exclusive *)

  datatype lit =
      LInt of string
    | LWord of string
    | LReal of string
    | LChar of string     (* decoded, exactly one character *)
    | LString of string   (* decoded contents *)

  datatype ty =
      TyVar of tyvar
    | TyCon of ty list * ident          (* (), [t], or (t1,..,tn) before a con *)
    | TyTuple of ty list                (* t1 * .. * tn, n >= 2 *)
    | TyArrow of ty * ty
    | TyRecord of (string * ty) list

  (* patterns: a `patnode` paired with its source span *)
  datatype patnode =
      PWild
    | PVar of ident
    | PLit of lit
    | PTuple of pat list
    | PList of pat list
    | PRecord of (string * pat) list * bool   (* fields, flexible (...) ? *)
    | PCon of ident * pat                       (* constructor application *)
    | PInfix of ident * pat * pat               (* infixed constructor, e.g. :: *)
    | PTyped of pat * ty
    | PAs of ident * pat
  withtype pat = patnode * span

  (* expressions / declarations / specs carry spans; structures and signatures
     do not (they are containers). All are mutually recursive. *)
  datatype expnode =
      ELit of lit
    | EVar of ident
    | ETuple of exp list                  (* () is ETuple [] *)
    | EList of exp list
    | ERecord of (string * exp) list
    | ESelector of string                 (* #lab *)
    | ESeq of exp list                    (* (e1; e2; ..) *)
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
      DVal of tyvar list * (pat * exp) list * bool      (* tyvars, binds, rec? *)
    | DFun of tyvar list
              * (ident * { pats : pat list, ret : ty option, body : exp } list) list
    | DType of (tyvar list * ident * ty) list
    | DDatatype of
        { tyvars : tyvar list, name : ident, cons : (ident * ty option) list } list
        * (tyvar list * ident * ty) list                (* datbinds + withtype *)
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
    | StrConstraint of strexp * sigexp * bool    (* opaque (:>) ? *)

  and sigexp =
      SigSig of spec list
    | SigId of ident
    | SigWhere of sigexp * (tyvar list * ident * ty) list   (* where type *)

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

  (* convenience aliases for the inlined record shapes above *)
  type fclause = { pats : pat list, ret : ty option, body : exp }
  type datbind = { tyvars : tyvar list, name : ident,
                   cons : (ident * ty option) list }
  type fctbind = { name : ident, arg : ident, argSig : sigexp,
                   ascription : (bool * sigexp) option, body : strexp }

  type program = dec list

  (* span accessors / constructors for the wrapped node types *)
  val expSpan  : exp -> span
  val patSpan  : pat -> span
  val decSpan  : dec -> span
  val specSpan : spec -> span

  val expNode  : exp -> expnode
  val patNode  : pat -> patnode
  val decNode  : dec -> decnode
  val specNode : spec -> specnode

  (* erase all spans to a canonical zero, for span-insensitive AST equality *)
  val eraseExp     : exp -> exp
  val erasePat     : pat -> pat
  val eraseDec     : dec -> dec
  val eraseSpec    : spec -> spec
  val eraseProgram : program -> program
end
