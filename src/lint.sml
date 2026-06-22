(* lint.sml - see lint.sig.

   A pure, deterministic syntax/scope-level linter over the positioned sml-mlast
   AST plus the lexical token stream. No type inference, no I/O, no mutation of
   inputs. Every rule is best-effort but tuned for HIGH PRECISION: when a fact
   cannot be established from the parse tree alone, the rule stays silent rather
   than emitting a false positive. *)

structure Lint :> LINT =
struct
  open Ast

  type pos  = Pos.pos
  type span = Pos.span

  datatype severity = Error | Warning | Hint

  type diagnostic =
    { span : span, severity : severity, rule : string
    , message : string, fix : string option }

  exception Parse of string

  val rules =
    ["unused", "shadow", "dead-open", "nonexhaustive", "redundant-parens",
     "naming"]

  fun severityToString Error   = "error"
    | severityToString Warning = "warning"
    | severityToString Hint    = "hint"

  fun diagToString (d : diagnostic) =
    Pos.spanToString (#span d) ^ " [" ^ severityToString (#severity d) ^ "] "
    ^ #rule d ^ ": " ^ #message d

  (* ----------------------------------------------------------------- *)
  (* small utilities                                                   *)
  (* ----------------------------------------------------------------- *)

  fun elem (x, xs) = List.exists (fn y => y = x) xs
  fun dedup xs = List.foldr (fn (x, a) => if elem (x, a) then a else x :: a) [] xs

  fun firstChar s = if s = "" then NONE else SOME (String.sub (s, 0))
  fun startsUpperAlpha s =
    case firstChar s of SOME c => Char.isUpper c | NONE => false
  fun startsLowerAlpha s =
    case firstChar s of SOME c => Char.isLower c | NONE => false
  fun startsUnderscore s = String.isPrefix "_" s

  fun dotIndex s =
    let
      val n = String.size s
      fun go i = if i >= n then NONE
                 else if String.sub (s, i) = #"." then SOME i
                 else go (i + 1)
    in go 0 end
  fun isQualified s = Option.isSome (dotIndex s)
  fun qualHead s =
    case dotIndex s of SOME i => String.substring (s, 0, i) | NONE => s

  (* SML basis pervasives: bare identifiers that resolve to the top-level basis
     and therefore can never have come from an `open`. Omissions are SAFE -
     they only reduce the recall of the dead-open rule, never its precision. *)
  val pervasive =
    [ "+", "-", "*", "/", "div", "mod", "quot", "rem", "~", "abs",
      "<", ">", "<=", ">=", "=", "<>", "::", "@", "^", "o", "!", ":=",
      "before", "ignore", "true", "false", "nil", "NONE", "SOME",
      "LESS", "EQUAL", "GREATER", "ref",
      "print", "map", "app", "foldl", "foldr", "rev", "length", "hd", "tl",
      "null", "explode", "implode", "concat", "size", "substring", "str",
      "ord", "chr", "not", "real", "floor", "ceil", "round", "trunc",
      "valOf", "isSome", "getOpt", "vector", "use", "exnName", "exnMessage" ]

  val builtinCons =
    ["true", "false", "nil", "NONE", "SOME", "LESS", "EQUAL", "GREATER",
     "ref", "::"]

  (* ----------------------------------------------------------------- *)
  (* deep collectors: every dec node and every exp node in the program *)
  (* ----------------------------------------------------------------- *)

  fun caExp (e, (ds, es)) =
    let
      val es = e :: es
      val acc = (ds, es)
    in
      case expNode e of
        ELit _ => acc
      | EVar _ => acc
      | ESelector _ => acc
      | ETuple xs => caList (xs, acc)
      | EList xs => caList (xs, acc)
      | ERecord fs => caList (List.map #2 fs, acc)
      | ESeq xs => caList (xs, acc)
      | EApp (a, b) => caExp (b, caExp (a, acc))
      | EInfix (_, a, b) => caExp (b, caExp (a, acc))
      | ETyped (a, _) => caExp (a, acc)
      | EAndalso (a, b) => caExp (b, caExp (a, acc))
      | EOrelse (a, b) => caExp (b, caExp (a, acc))
      | EHandle (a, ms) => caArms (ms, caExp (a, acc))
      | ERaise a => caExp (a, acc)
      | EIf (a, b, c) => caExp (c, caExp (b, caExp (a, acc)))
      | EWhile (a, b) => caExp (b, caExp (a, acc))
      | ECase (a, ms) => caArms (ms, caExp (a, acc))
      | EFn ms => caArms (ms, acc)
      | ELet (decs, b) => caExp (b, caDecs (decs, acc))
    end
  and caList (xs, acc) = List.foldl caExp acc xs
  and caArms (ms, acc) = List.foldl (fn ((_, e), a) => caExp (e, a)) acc ms
  and caDec (d, (ds, es)) =
    let
      val ds = d :: ds
      val acc = (ds, es)
    in
      case decNode d of
        DVal (_, binds, _) =>
          List.foldl (fn ((_, e), a) => caExp (e, a)) acc binds
      | DFun (_, funs) =>
          List.foldl
            (fn ((_, cls), a) =>
               List.foldl (fn ({body, ...}, a2) => caExp (body, a2)) a cls)
            acc funs
      | DLocal (d1, d2) => caDecs (d2, caDecs (d1, acc))
      | DStructure binds =>
          List.foldl (fn ((_, se), a) => caStr (se, a)) acc binds
      | DFunctor binds =>
          List.foldl (fn (fb, a) => caStr (#body fb, a)) acc binds
      | _ => acc
    end
  and caDecs (decs, acc) = List.foldl caDec acc decs
  and caStr (se, acc) =
    case se of
      StrStruct decs => caDecs (decs, acc)
    | StrLet (decs, se') => caStr (se', caDecs (decs, acc))
    | StrConstraint (se', _, _) => caStr (se', acc)
    | _ => acc

  fun collectAll prog = caDecs (prog, ([], []))

  (* datatype/constructor information, gathered from the whole file *)
  fun datatypeInfo allDecs =
    let
      fun ofDec (d, (dts, cons, excs)) =
        case decNode d of
          DDatatype (dbs, _) =>
            List.foldl
              (fn (db, (dts, cons, excs)) =>
                 let val cs = List.map #1 (#cons db)
                 in (( #name db, cs) :: dts,
                     List.map (fn c => (c, #name db)) cs @ cons, excs)
                 end)
              (dts, cons, excs) dbs
        | DException ebs => (dts, cons, List.map #1 ebs @ excs)
        | _ => (dts, cons, excs)
      val (dts, owners, excs) = List.foldl ofDec ([], [], []) allDecs
      val conNames = List.map #1 owners
      val conSet = dedup (builtinCons @ conNames @ excs)
    in { datatypes = dts, owner = owners, conSet = conSet } end

  (* ----------------------------------------------------------------- *)
  (* binding-variable extraction from patterns                         *)
  (* ----------------------------------------------------------------- *)

  (* The value variables a pattern binds, EXCLUDING anything that is (or might
     be) a constructor: known constructors, and - conservatively - any
     capitalised name (these are almost always nullary constructors, possibly
     imported, and treating them as bindings would risk false positives). *)
  fun patVars conSet p acc =
    case patNode p of
      PWild => acc
    | PVar v =>
        if elem (v, conSet) orelse elem (v, builtinCons) orelse startsUpperAlpha v
        then acc else (v, patSpan p) :: acc
    | PLit _ => acc
    | PTuple ps => List.foldl (fn (p, a) => patVars conSet p a) acc ps
    | PList ps => List.foldl (fn (p, a) => patVars conSet p a) acc ps
    | PRecord (fs, _) =>
        List.foldl (fn ((_, p), a) => patVars conSet p a) acc fs
    | PCon (_, p) => patVars conSet p acc
    | PInfix (_, a, b) => patVars conSet b (patVars conSet a acc)
    | PTyped (p, _) => patVars conSet p acc
    | PAs (v, p) =>
        let val acc' = patVars conSet p acc
        in if elem (v, conSet) orelse startsUpperAlpha v then acc'
           else (v, patSpan p) :: acc'
        end

  (* ----------------------------------------------------------------- *)
  (* reference collection (all identifiers USED, over-approximating)   *)
  (* ----------------------------------------------------------------- *)

  fun refsTy (t, acc) =
    case t of
      TyVar _ => acc
    | TyCon (ts, id) => List.foldl refsTy (id :: acc) ts
    | TyTuple ts => List.foldl refsTy acc ts
    | TyArrow (a, b) => refsTy (b, refsTy (a, acc))
    | TyRecord fs => List.foldl (fn ((_, t), a) => refsTy (t, a)) acc fs

  fun refsPat (p, acc) =
    case patNode p of
      PWild => acc
    | PVar _ => acc
    | PLit _ => acc
    | PTuple ps => List.foldl refsPat acc ps
    | PList ps => List.foldl refsPat acc ps
    | PRecord (fs, _) => List.foldl (fn ((_, p), a) => refsPat (p, a)) acc fs
    | PCon (c, p) => refsPat (p, c :: acc)
    | PInfix (c, a, b) => refsPat (b, refsPat (a, c :: acc))
    | PTyped (p, t) => refsTy (t, refsPat (p, acc))
    | PAs (_, p) => refsPat (p, acc)

  fun refsExp (e, acc) =
    case expNode e of
      ELit _ => acc
    | EVar id => id :: acc
    | ESelector _ => acc
    | ETuple xs => List.foldl refsExp acc xs
    | EList xs => List.foldl refsExp acc xs
    | ERecord fs => List.foldl (fn ((_, e), a) => refsExp (e, a)) acc fs
    | ESeq xs => List.foldl refsExp acc xs
    | EApp (a, b) => refsExp (b, refsExp (a, acc))
    | EInfix (id, a, b) => refsExp (b, refsExp (a, id :: acc))
    | ETyped (a, t) => refsTy (t, refsExp (a, acc))
    | EAndalso (a, b) => refsExp (b, refsExp (a, acc))
    | EOrelse (a, b) => refsExp (b, refsExp (a, acc))
    | EHandle (a, ms) => refsArms (ms, refsExp (a, acc))
    | ERaise a => refsExp (a, acc)
    | EIf (a, b, c) => refsExp (c, refsExp (b, refsExp (a, acc)))
    | EWhile (a, b) => refsExp (b, refsExp (a, acc))
    | ECase (a, ms) => refsArms (ms, refsExp (a, acc))
    | EFn ms => refsArms (ms, acc)
    | ELet (ds, b) => refsExp (b, refsDecs (ds, acc))
  and refsArms (ms, acc) =
    List.foldl (fn ((p, e), a) => refsExp (e, refsPat (p, a))) acc ms
  and refsDec (d, acc) =
    case decNode d of
      DVal (_, binds, _) =>
        List.foldl (fn ((p, e), a) => refsExp (e, refsPat (p, a))) acc binds
    | DFun (_, funs) =>
        List.foldl
          (fn ((_, cls), a) =>
             List.foldl
               (fn ({pats, ret, body}, a2) =>
                  let
                    val a3 = List.foldl refsPat a2 pats
                    val a4 = case ret of SOME t => refsTy (t, a3) | NONE => a3
                  in refsExp (body, a4) end)
               a cls)
          acc funs
    | DType binds => List.foldl (fn ((_, _, t), a) => refsTy (t, a)) acc binds
    | DDatatype (dbs, withs) =>
        let
          val a1 =
            List.foldl
              (fn (db, a) =>
                 List.foldl
                   (fn ((_, SOME t), a2) => refsTy (t, a2) | ((_, NONE), a2) => a2)
                   a (#cons db))
              acc dbs
        in List.foldl (fn ((_, _, t), a) => refsTy (t, a)) a1 withs end
    | DException ebs =>
        List.foldl
          (fn ((_, SOME t), a) => refsTy (t, a) | ((_, NONE), a) => a) acc ebs
    | DOpen _ => acc
    | DLocal (d1, d2) => refsDecs (d2, refsDecs (d1, acc))
    | DInfix _ => acc
    | DInfixr _ => acc
    | DNonfix _ => acc
    | DStructure binds => List.foldl (fn ((_, se), a) => refsStr (se, a)) acc binds
    | DSignature _ => acc
    | DFunctor binds => List.foldl (fn (fb, a) => refsStr (#body fb, a)) acc binds
  and refsDecs (ds, acc) = List.foldl refsDec acc ds
  and refsStr (se, acc) =
    case se of
      StrStruct ds => refsDecs (ds, acc)
    | StrId _ => acc
    | StrApp (_, se') => refsStr (se', acc)
    | StrLet (ds, se') => refsStr (se', refsDecs (ds, acc))
    | StrConstraint (se', _, _) => refsStr (se', acc)

  (* ----------------------------------------------------------------- *)
  (* rule: unused bindings                                             *)
  (* ----------------------------------------------------------------- *)

  fun unusedBindings prog =
    let
      val (allDecs, _) = collectAll prog
      val { conSet, ... } = datatypeInfo allDecs
      val out = ref ([] : diagnostic list)
      fun emit (sp, sev, msg) =
        out := { span = sp, severity = sev, rule = "unused",
                 message = msg, fix = NONE } :: !out
      fun flagVar sev kind (v, sp) =
        if startsUnderscore v then ()
        else emit (sp, sev, "unused " ^ kind ^ " `" ^ v ^ "`")

      fun vArm (p, e) =
        let
          val used = refsExp (e, [])
          val vars = patVars conSet p []
        in
          List.app (fn (v, sp) =>
                      if elem (v, used) then () else flagVar Hint "variable" (v, sp))
                   vars;
          vExp e
        end
      and vExp e =
        case expNode e of
          ELit _ => ()
        | EVar _ => ()
        | ESelector _ => ()
        | ETuple xs => List.app vExp xs
        | EList xs => List.app vExp xs
        | ERecord fs => List.app (fn (_, e) => vExp e) fs
        | ESeq xs => List.app vExp xs
        | EApp (a, b) => (vExp a; vExp b)
        | EInfix (_, a, b) => (vExp a; vExp b)
        | ETyped (a, _) => vExp a
        | EAndalso (a, b) => (vExp a; vExp b)
        | EOrelse (a, b) => (vExp a; vExp b)
        | EHandle (a, ms) => (vExp a; List.app vArm ms)
        | ERaise a => vExp a
        | EIf (a, b, c) => (vExp a; vExp b; vExp c)
        | EWhile (a, b) => (vExp a; vExp b)
        | ECase (a, ms) => (vExp a; List.app vArm ms)
        | EFn ms => List.app vArm ms
        | ELet (ds, body) => vLet (ds, body)
      and vLet (ds, body) =
        let
          val used = refsDecs (ds, refsExp (body, []))
        in
          List.app (flagLetDec used) ds;
          List.app vDec ds;
          vExp body
        end
      and flagLetDec used d =
        case decNode d of
          DVal (_, binds, _) =>
            List.app
              (fn (p, _) =>
                 List.app
                   (fn (v, sp) =>
                      if elem (v, used) then ()
                      else flagVar Warning "value" (v, sp))
                   (patVars conSet p []))
              binds
        | DFun (_, funs) =>
            List.app
              (fn (name, _) =>
                 if elem (name, conSet) orelse startsUpperAlpha name
                    orelse startsUnderscore name then ()
                 else if elem (name, used) then ()
                 else emit (decSpan d, Warning,
                            "unused function `" ^ name ^ "`"))
              funs
        | _ => ()
      and vDec d =
        case decNode d of
          DVal (_, binds, _) => List.app (fn (_, e) => vExp e) binds
        | DFun (_, funs) =>
            List.app
              (fn (_, cls) =>
                 List.app
                   (fn {pats, body, ...} =>
                      let
                        val used = refsExp (body, [])
                        val vars =
                          List.concat (List.map (fn p => patVars conSet p []) pats)
                      in
                        List.app
                          (fn (v, sp) =>
                             if elem (v, used) then ()
                             else flagVar Hint "parameter" (v, sp))
                          vars;
                        vExp body
                      end)
                   cls)
              funs
        | DLocal (d1, d2) =>
            let
              val used = refsDecs (d2, refsDecs (d1, []))
            in
              List.app (flagLetDec used) d1;
              List.app vDec d1;
              List.app vDec d2
            end
        | DStructure binds => List.app (fn (_, se) => vStr se) binds
        | DFunctor binds => List.app (fn fb => vStr (#body fb)) binds
        | _ => ()
      and vStr se =
        case se of
          StrStruct ds => List.app vDec ds
        | StrLet (ds, se') => (List.app vDec ds; vStr se')
        | StrConstraint (se', _, _) => vStr se'
        | _ => ()
    in
      List.app vDec prog;
      List.rev (!out)
    end

  (* ----------------------------------------------------------------- *)
  (* rule: shadowing                                                   *)
  (* ----------------------------------------------------------------- *)

  fun shadowing prog =
    let
      val (allDecs, _) = collectAll prog
      val { conSet, ... } = datatypeInfo allDecs
      val out = ref ([] : diagnostic list)
      fun emit (sp, v) =
        out := { span = sp, severity = Warning, rule = "shadow",
                 message = "binding of `" ^ v ^ "` shadows an earlier binding",
                 fix = NONE } :: !out
      (* introduce names: flag those already in env, then extend env *)
      fun intro env vs =
        List.foldl
          (fn ((v, sp), e) => (if elem (v, e) then emit (sp, v) else (); v :: e))
          env vs
      fun extend env vs = List.foldl (fn ((v, _), e) => v :: e) env vs
      fun bindPat env p = intro env (patVars conSet p [])

      fun sExp env e =
        case expNode e of
          ELit _ => ()
        | EVar _ => ()
        | ESelector _ => ()
        | ETuple xs => List.app (sExp env) xs
        | EList xs => List.app (sExp env) xs
        | ERecord fs => List.app (fn (_, e) => sExp env e) fs
        | ESeq xs => List.app (sExp env) xs
        | EApp (a, b) => (sExp env a; sExp env b)
        | EInfix (_, a, b) => (sExp env a; sExp env b)
        | ETyped (a, _) => sExp env a
        | EAndalso (a, b) => (sExp env a; sExp env b)
        | EOrelse (a, b) => (sExp env a; sExp env b)
        | EHandle (a, ms) => (sExp env a; List.app (sArm env) ms)
        | ERaise a => sExp env a
        | EIf (a, b, c) => (sExp env a; sExp env b; sExp env c)
        | EWhile (a, b) => (sExp env a; sExp env b)
        | ECase (a, ms) => (sExp env a; List.app (sArm env) ms)
        | EFn ms => List.app (sArm env) ms
        | ELet (ds, body) => let val env' = sDecs env ds in sExp env' body end
      and sArm env (p, e) = let val env' = bindPat env p in sExp env' e end
      and sDec env d =
        case decNode d of
          DVal (_, binds, _) =>
            (List.app (fn (_, e) => sExp env e) binds;
             intro env
               (List.concat
                  (List.map (fn (p, _) => patVars conSet p []) binds)))
        | DFun (_, funs) =>
            let
              val names =
                List.mapPartial
                  (fn (nm, _) =>
                     if elem (nm, conSet) orelse startsUpperAlpha nm then NONE
                     else SOME (nm, decSpan d))
                  funs
              val env1 = intro env names
            in
              List.app
                (fn (_, cls) =>
                   List.app
                     (fn {pats, body, ...} =>
                        let val envB = List.foldl (fn (p, e) => bindPat e p) env1 pats
                        in sExp envB body end)
                     cls)
                funs;
              env1
            end
        | DLocal (d1, d2) =>
            let
              val env1 = sDecs env d1
              val _ = sDecs env1 d2
              val exports =
                List.concat (List.map (fn d => bound1 conSet d) d2)
            in extend env exports end
        | DStructure binds =>
            (List.app (fn (_, se) => sStr env se) binds; env)
        | DFunctor binds =>
            (List.app (fn fb => sStr env (#body fb)) binds; env)
        | _ => env
      and sDecs env ds = List.foldl (fn (d, e) => sDec e d) env ds
      and sStr env se =
        case se of
          StrStruct ds => ignore (sDecs env ds)
        | StrLet (ds, se') => let val e = sDecs env ds in sStr e se' end
        | StrConstraint (se', _, _) => sStr env se'
        | _ => ()
    in
      ignore (sDecs [] prog);
      List.rev (!out)
    end

  (* names bound by a single dec at its own level (value variables) *)
  and bound1 conSet d =
    case decNode d of
      DVal (_, binds, _) =>
        List.concat (List.map (fn (p, _) => patVars conSet p []) binds)
    | DFun (_, funs) =>
        List.mapPartial
          (fn (nm, _) =>
             if elem (nm, conSet) orelse startsUpperAlpha nm then NONE
             else SOME (nm, decSpan d))
          funs
    | DLocal (_, d2) => List.concat (List.map (fn d => bound1 conSet d) d2)
    | _ => []

  (* ----------------------------------------------------------------- *)
  (* rule: dead open                                                   *)
  (* ----------------------------------------------------------------- *)

  fun deadOpen prog =
    let
      val (allDecs, _) = collectAll prog
      val { conSet, ... } = datatypeInfo allDecs
      val out = ref ([] : diagnostic list)
      fun emit (sp, m) =
        out := { span = sp, severity = Hint, rule = "dead-open",
                 message = "unused open of `" ^ m
                           ^ "` (no qualified or free reference to it)",
                 fix = NONE } :: !out

      (* free-variable analysis of a region (decls + optional body) given a
         starting environment of in-scope names. Returns the set of qualifier
         heads used qualified, and the set of free bare identifiers. *)
      fun regionFree (env0, decs, bodyOpt) =
        let
          val qualsR = ref ([] : string list)
          val freeR = ref ([] : string list)
          fun useId env id =
            if isQualified id then qualsR := qualHead id :: !qualsR
            else if elem (id, env) orelse elem (id, pervasive)
                    orelse elem (id, conSet) then ()
            else freeR := id :: !freeR
          fun fePat env p =
            case patNode p of
              PWild => env
            | PVar v =>
                if elem (v, conSet) orelse elem (v, pervasive) then env
                else if startsUpperAlpha v then (useId env v; env)
                else v :: env
            | PLit _ => env
            | PTuple ps => List.foldl (fn (p, e) => fePat e p) env ps
            | PList ps => List.foldl (fn (p, e) => fePat e p) env ps
            | PRecord (fs, _) =>
                List.foldl (fn ((_, p), e) => fePat e p) env fs
            | PCon (c, p) => (useId env c; fePat env p)
            | PInfix (c, a, b) => (useId env c; fePat (fePat env a) b)
            | PTyped (p, _) => fePat env p
            | PAs (v, p) =>
                let val e = fePat env p
                in if elem (v, conSet) orelse startsUpperAlpha v then e
                   else v :: e end
          fun feExp env e =
            case expNode e of
              ELit _ => ()
            | EVar id => useId env id
            | ESelector _ => ()
            | ETuple xs => List.app (feExp env) xs
            | EList xs => List.app (feExp env) xs
            | ERecord fs => List.app (fn (_, e) => feExp env e) fs
            | ESeq xs => List.app (feExp env) xs
            | EApp (a, b) => (feExp env a; feExp env b)
            | EInfix (id, a, b) => (useId env id; feExp env a; feExp env b)
            | ETyped (a, _) => feExp env a
            | EAndalso (a, b) => (feExp env a; feExp env b)
            | EOrelse (a, b) => (feExp env a; feExp env b)
            | EHandle (a, ms) => (feExp env a; List.app (feArm env) ms)
            | ERaise a => feExp env a
            | EIf (a, b, c) => (feExp env a; feExp env b; feExp env c)
            | EWhile (a, b) => (feExp env a; feExp env b)
            | ECase (a, ms) => (feExp env a; List.app (feArm env) ms)
            | EFn ms => List.app (feArm env) ms
            | ELet (ds, b) => let val env' = feDecs env ds in feExp env' b end
          and feArm env (p, e) = let val env' = fePat env p in feExp env' e end
          and feDec env d =
            case decNode d of
              DVal (_, binds, _) =>
                (List.app (fn (_, e) => feExp env e) binds;
                 List.foldl (fn ((p, _), e) => fePat e p) env binds)
            | DFun (_, funs) =>
                let
                  val names =
                    List.mapPartial
                      (fn (nm, _) =>
                         if elem (nm, conSet) orelse startsUpperAlpha nm
                         then NONE else SOME nm)
                      funs
                  val env1 = names @ env
                in
                  List.app
                    (fn (_, cls) =>
                       List.app
                         (fn {pats, body, ...} =>
                            let
                              val envB =
                                List.foldl (fn (p, e) => fePat e p) env1 pats
                            in feExp envB body end)
                         cls)
                    funs;
                  env1
                end
            | DLocal (d1, d2) => let val e1 = feDecs env d1 in feDecs e1 d2 end
            | DStructure binds =>
                (List.app (fn (_, se) => feStr env se) binds; env)
            | DFunctor binds =>
                (List.app (fn fb => feStr env (#body fb)) binds; env)
            | _ => env
          and feDecs env ds = List.foldl (fn (d, e) => feDec e d) env ds
          and feStr env se =
            case se of
              StrStruct ds => ignore (feDecs env ds)
            | StrLet (ds, se') => let val e = feDecs env ds in feStr e se' end
            | StrConstraint (se', _, _) => feStr env se'
            | _ => ()
          val envAfter = feDecs env0 decs
          val () = case bodyOpt of SOME b => feExp envAfter b | NONE => ()
        in (dedup (!qualsR), dedup (!freeR)) end

      (* walk the program to find every `open`, tracking the in-scope env and
         the remainder of its enclosing scope. *)
      fun boundList env decs =
        List.concat (List.map (fn d => List.map #1 (bound1 conSet d)) decs)
        @ env
      fun walkDecs env decs bodyOpt =
        let
          fun loop (pre, post) =
            case post of
              [] => ()
            | d :: rest =>
                ((case decNode d of
                    DOpen ms =>
                      let
                        val envAt = boundList env (List.rev pre)
                        fun chk m =
                          let val (quals, free) = regionFree (envAt, rest, bodyOpt)
                          in if not (elem (m, quals)) andalso null free
                             then emit (decSpan d, m) else () end
                      in List.app chk ms end
                  | _ => ());
                 walkDec env d;
                 loop (d :: pre, rest))
        in loop ([], decs) end
      and walkDec env d =
        case decNode d of
          DVal (_, binds, _) => List.app (fn (_, e) => walkExp env e) binds
        | DFun (_, funs) =>
            List.app
              (fn (_, cls) =>
                 List.app
                   (fn {pats, body, ...} =>
                      let
                        val env' =
                          List.concat
                            (List.map (fn p => List.map #1 (patVars conSet p [])) pats)
                          @ env
                      in walkExp env' body end)
                   cls)
              funs
        | DLocal (d1, d2) =>
            (walkDecs env d1 NONE;
             walkDecs (boundList env d1) d2 NONE)
        | DStructure binds => List.app (fn (_, se) => walkStr env se) binds
        | DFunctor binds => List.app (fn fb => walkStr env (#body fb)) binds
        | _ => ()
      and walkExp env e =
        case expNode e of
          ELit _ => ()
        | EVar _ => ()
        | ESelector _ => ()
        | ETuple xs => List.app (walkExp env) xs
        | EList xs => List.app (walkExp env) xs
        | ERecord fs => List.app (fn (_, e) => walkExp env e) fs
        | ESeq xs => List.app (walkExp env) xs
        | EApp (a, b) => (walkExp env a; walkExp env b)
        | EInfix (_, a, b) => (walkExp env a; walkExp env b)
        | ETyped (a, _) => walkExp env a
        | EAndalso (a, b) => (walkExp env a; walkExp env b)
        | EOrelse (a, b) => (walkExp env a; walkExp env b)
        | EHandle (a, ms) => (walkExp env a; List.app (walkArm env) ms)
        | ERaise a => walkExp env a
        | EIf (a, b, c) => (walkExp env a; walkExp env b; walkExp env c)
        | EWhile (a, b) => (walkExp env a; walkExp env b)
        | ECase (a, ms) => (walkExp env a; List.app (walkArm env) ms)
        | EFn ms => List.app (walkArm env) ms
        | ELet (ds, body) => walkDecs env ds (SOME body)
      and walkArm env (p, e) =
        let val env' = List.map #1 (patVars conSet p []) @ env
        in walkExp env' e end
      and walkStr env se =
        case se of
          StrStruct ds => walkDecs env ds NONE
        | StrLet (ds, se') => (walkDecs env ds NONE; walkStr (boundList env ds) se')
        | StrConstraint (se', _, _) => walkStr env se'
        | _ => ()
    in
      walkDecs [] prog NONE;
      List.rev (!out)
    end

  (* ----------------------------------------------------------------- *)
  (* rule: non-exhaustive match (datatypes declared in the same file)  *)
  (* ----------------------------------------------------------------- *)

  fun nonExhaustive prog =
    let
      val (allDecs, allExps) = collectAll prog
      val info = datatypeInfo allDecs
      val owner = #owner info
      val datatypes = #datatypes info
      val out = ref ([] : diagnostic list)
      fun ownerOf c =
        case List.find (fn (k, _) => k = c) owner of
          SOME (_, t) => SOME t | NONE => NONE
      fun consOf t =
        case List.find (fn (k, _) => k = t) datatypes of
          SOME (_, cs) => cs | NONE => []

      (* CATCHALL = arm definitely matches anything; OTHER = pattern we don't
         analyze; CON c = a constructor head *)
      datatype head = CATCHALL | OTHER | CON of string
      fun headOf p =
        case patNode p of
          PWild => CATCHALL
        | PVar v => (case ownerOf v of SOME _ => CON v | NONE => CATCHALL)
        | PCon (c, _) => CON c
        | PInfix (c, _, _) => CON c
        | PAs (_, p) => headOf p
        | PTyped (p, _) => headOf p
        | _ => OTHER

      fun analyze (arms, sp) =
        let
          val heads = List.map (fn (p, _) => headOf p) arms
        in
          if List.exists (fn CATCHALL => true | _ => false) heads then ()
          else if List.exists (fn OTHER => true | _ => false) heads then ()
          else
            let
              val cons = List.mapPartial (fn CON c => SOME c | _ => NONE) heads
            in
              case cons of
                [] => ()
              | c0 :: _ =>
                  (case ownerOf c0 of
                     NONE => ()
                   | SOME t =>
                       if List.all (fn c => ownerOf c = SOME t) cons then
                         let
                           val full = consOf t
                           val covered = dedup cons
                           val missing =
                             List.filter (fn c => not (elem (c, covered))) full
                         in
                           if null missing then ()
                           else
                             out :=
                               { span = sp, severity = Warning,
                                 rule = "nonexhaustive",
                                 message =
                                   "non-exhaustive match on datatype `" ^ t
                                   ^ "`: missing "
                                   ^ String.concatWith ", "
                                       (List.map (fn c => "`" ^ c ^ "`") missing),
                                 fix = NONE } :: !out
                         end
                       else ())
            end
        end
    in
      List.app
        (fn e =>
           case expNode e of
             ECase (_, arms) => analyze (arms, expSpan e)
           | EFn arms => analyze (arms, expSpan e)
           | _ => ())
        allExps;
      List.rev (!out)
    end

  (* ----------------------------------------------------------------- *)
  (* rule: naming conventions                                          *)
  (* ----------------------------------------------------------------- *)

  fun naming prog =
    let
      val (allDecs, _) = collectAll prog
      val { conSet, ... } = datatypeInfo allDecs
      val out = ref ([] : diagnostic list)
      fun emit (sp, msg) =
        out := { span = sp, severity = Hint, rule = "naming",
                 message = msg, fix = NONE } :: !out
      fun checkDec d =
        case decNode d of
          DVal (_, binds, _) =>
            List.app
              (fn (p, _) =>
                 case patNode p of
                   PVar v =>
                     if not (elem (v, conSet)) andalso startsUpperAlpha v
                     then emit (patSpan p,
                                "value `" ^ v
                                ^ "` should start with a lower-case letter")
                     else ()
                 | _ => ())
              binds
        | DFun (_, funs) =>
            List.app
              (fn (name, _) =>
                 if not (elem (name, conSet)) andalso startsUpperAlpha name
                 then emit (decSpan d,
                            "function `" ^ name
                            ^ "` should start with a lower-case letter")
                 else ())
              funs
        | DDatatype (dbs, _) =>
            List.app
              (fn db =>
                 List.app
                   (fn (c, _) =>
                      if startsLowerAlpha c then
                        emit (decSpan d,
                              "constructor `" ^ c
                              ^ "` should start with an upper-case letter")
                      else ())
                   (#cons db))
              dbs
        | DException ebs =>
            List.app
              (fn (c, _) =>
                 if startsLowerAlpha c then
                   emit (decSpan d,
                         "exception `" ^ c
                         ^ "` should start with an upper-case letter")
                 else ())
              ebs
        | _ => ()
    in
      List.app checkDec allDecs;
      List.rev (!out)
    end

  (* ----------------------------------------------------------------- *)
  (* rule: redundant parentheses (token-level)                         *)
  (* ----------------------------------------------------------------- *)

  fun redundantParens src =
    let
      val toks =
        Vector.fromList (Lexer.tokenize src)
        handle Lexer.Lex m => raise Parse m
      val n = Vector.length toks
      fun tk i = #1 (Vector.sub (toks, i))
      fun sp i = #2 (Vector.sub (toks, i))
      fun isLP i = (case tk i of Token.LPAREN => true | _ => false)
      fun isRP i = (case tk i of Token.RPAREN => true | _ => false)
      fun isAtom i =
        case tk i of
          Token.ID _ => true | Token.INT _ => true | Token.WORD _ => true
        | Token.REAL _ => true | Token.CHAR _ => true | Token.STRING _ => true
        | Token.TYVAR _ => true | _ => false

      (* matching of parentheses: match[i] = index of partner, or ~1 *)
      val match = Array.array (n, ~1)
      fun buildMatch (i, stack) =
        if i >= n then ()
        else
          (case tk i of
             Token.LPAREN => buildMatch (i + 1, i :: stack)
           | Token.RPAREN =>
               (case stack of
                  j :: rest =>
                    (Array.update (match, i, j);
                     Array.update (match, j, i);
                     buildMatch (i + 1, rest))
                | [] => buildMatch (i + 1, []))
           | _ => buildMatch (i + 1, stack))
      val () = buildMatch (0, [])

      val out = ref ([] : diagnostic list)
      fun emit (lo, hi, msg, fx) =
        out := { span = Pos.mkSpan (#lo (sp lo), #hi (sp hi)),
                 severity = Hint, rule = "redundant-parens",
                 message = msg, fix = fx } :: !out

      fun scan i =
        if i >= n then ()
        else
          let
            val m = Array.sub (match, i)
          in
            if isLP i andalso i + 2 < n andalso isAtom (i + 1)
               andalso isRP (i + 2) andalso m = i + 2 then
              emit (i, i + 2,
                    "redundant parentheses around `"
                    ^ Token.toString (tk (i + 1)) ^ "`",
                    SOME (Token.toString (tk (i + 1))))
            else if isLP i andalso m >= 0 andalso i + 1 < m
                    andalso isLP (i + 1) andalso Array.sub (match, i + 1) = m - 1
            then
              emit (i, m, "redundant nested parentheses", NONE)
            else ();
            scan (i + 1)
          end
    in
      scan 0;
      List.rev (!out)
    end

  (* ----------------------------------------------------------------- *)
  (* assembly                                                          *)
  (* ----------------------------------------------------------------- *)

  fun lintProgram prog =
    unusedBindings prog
    @ shadowing prog
    @ deadOpen prog
    @ nonExhaustive prog
    @ naming prog

  fun cmpPos ({line = l1, col = c1} : pos, {line = l2, col = c2} : pos) =
    case Int.compare (l1, l2) of EQUAL => Int.compare (c1, c2) | r => r
  fun cmpSpan ({lo = lo1, hi = hi1} : span, {lo = lo2, hi = hi2} : span) =
    case cmpPos (lo1, lo2) of EQUAL => cmpPos (hi1, hi2) | r => r
  fun sevRank Error = 0 | sevRank Warning = 1 | sevRank Hint = 2
  fun cmpDiag (d1 : diagnostic, d2 : diagnostic) =
    case cmpSpan (#span d1, #span d2) of
      EQUAL =>
        (case Int.compare (sevRank (#severity d1), sevRank (#severity d2)) of
           EQUAL =>
             (case String.compare (#rule d1, #rule d2) of
                EQUAL => String.compare (#message d1, #message d2)
              | r => r)
         | r => r)
    | r => r

  (* deterministic, stable merge sort *)
  fun sortDiags xs =
    let
      fun merge ([], ys) = ys
        | merge (xs, []) = xs
        | merge (x :: xs, y :: ys) =
            (case cmpDiag (x, y) of
               GREATER => y :: merge (x :: xs, ys)
             | _ => x :: merge (xs, y :: ys))
      fun split (x :: y :: zs) =
            let val (a, b) = split zs in (x :: a, y :: b) end
        | split zs = (zs, [])
      fun msort [] = []
        | msort [x] = [x]
        | msort xs = let val (a, b) = split xs in merge (msort a, msort b) end
    in msort xs end

  fun parseSrc src =
    Parser.parseString src
    handle Parser.Parse m => raise Parse m
         | Lexer.Lex m => raise Parse m

  fun lint src =
    let val prog = parseSrc src
    in sortDiags (lintProgram prog @ redundantParens src) end

  fun report src =
    let
      val ds = lint src
      val body =
        case ds of
          [] => "No issues found."
        | _ => String.concatWith "\n" (List.map diagToString ds)
      val nE = List.length (List.filter (fn d => #severity d = Error) ds)
      val nW = List.length (List.filter (fn d => #severity d = Warning) ds)
      val nH = List.length (List.filter (fn d => #severity d = Hint) ds)
      val summary =
        Int.toString (List.length ds) ^ " issue(s): "
        ^ Int.toString nE ^ " error(s), "
        ^ Int.toString nW ^ " warning(s), "
        ^ Int.toString nH ^ " hint(s)"
    in body ^ "\n\n" ^ summary ^ "\n" end
    handle Parse m => "parse error: " ^ m ^ "\n"
end
