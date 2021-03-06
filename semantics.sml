structure semantics :> semantics =
struct

open ast hash types common topsort ir translate temp

exception SemanFail

datatype EnvEntry =
    Var of { ty:TigerType, acc:Access, level:Level }
  | Func of { formals : types.TigerType list,
              ret : types.TigerType,
              extern : bool,
              label : string,
              level : Level }

val initLevel : translate.Level =
    newLevel { parent = outermost,
               name = "init_level",
               formals = [] }
val curLevel : translate.Level ref = ref initLevel

val labelStack = ref []

fun pushLoopLabel lab = labelStack := lab::(!labelStack)
fun popLoopLabel  ()  = labelStack := tl (!labelStack)
fun peekLoopLabel ()  = hd (!labelStack)

fun eseq (Skip, e) = e
  | eseq (s, e) = Eseq (s, e)

val glbStrings = ref []

fun opt_bind f NONE = NONE
  | opt_bind f (SOME v) = f v

fun find_idx n [] = NONE
  | find_idx n ((a,_)::xs) = if a = n then SOME 0 else opt_bind (fn x => SOME (x + 1)) (find_idx n xs)

fun addGlobalString s =
    let val lab = newlabel ()
        val _   = glbStrings := (lab, s)::(!glbStrings)
    in lab end

val init_venv : (symbol, EnvEntry) Tabla  = tabNew ()
val init_tenv : (symbol, TigerType) Tabla = tabNew ()

val _ = tabInsertList init_tenv [("int", TInt), ("string", TString)]
val _ = tabInsertList init_venv (map (fn (s,t) => (s, Func t)) runtime.func_list)

fun elem x [] = false
  | elem x (e::es) = x = e orelse elem x es

fun checkDups [] = false
  | checkDups (e::es) = if elem e es then true else checkDups es

fun semanErrorNoThrow info s = print ("Semantics: Error en "^(info2str info)^".\nSemantics: "^s^".\n")
fun semanError info s = ( semanErrorNoThrow info s ;
                          raise SemanFail )

fun uncurry f (x,y) = f x y

fun tipoReal ii t = case t of
    TReference r => ( case !r of
                        SOME tt => tipoReal ii tt
                        | NONE => semanError ii "error interno: referencia inválida (3)" )
  | otracosa => otracosa

fun typeMatch ii t1 t2 = case (t1, t2) of
    (TUnit, TUnit)     => true
  | (TNil, TRecord _)  => true
  | (TRecord _, TNil)  => true
  | (TRecord (_, u1), TRecord (_, u2))
       => u1 = u2
  | (TInt,   TInt)   => true
  | (TIntRO, TInt)   => true
  | (TInt,   TIntRO) => true
  | (TIntRO, TIntRO) => true
  | (TString, TString) => true
  | (TArray (_,u1), TArray (_,u2))
       => u1 = u2
  | (tt, TReference tref)
       => ( case !tref of
              NONE => semanError ii "error interno: referencia inválida (1)"
              | SOME t => typeMatch ii tt t
          )
  | (TReference tref, tt)
       => ( case !tref of
              NONE => semanError ii "error interno: referencia inválida (2)"
              | SOME t => typeMatch ii t tt
          )
  | (_,_) => false

fun seman vt tt exp =
 let fun seman' e = seman vt tt e
 in case exp of
   (* arrancamos con lo mas importante *)
    DebugE ee =>
        let
            fun pr1_type (n, ty) =
                print ("\t("^n^", "^(typeToString ty)^")\n")
            fun pr1_val (n, Var {ty, acc, level}) =
                print ("\t("^n^", Var {ty="^(typeToString ty)^"}\n")
              | pr1_val (n, Func {formals,ret,extern,label,level}) =
                let in
                    print ("\t("^n^", Func\n") ;
                    print ("\t\targs=\n") ;
                    List.app (fn t => print ("\t\t\t"^(typeToString t)^"\n")) formals ;
                    print ("\t\treturn= "^(typeToString ret)^"\n") ;
                    print ("\t\textern="^(if extern then "yes" else "no")^"\n") ;
                    print ("\t\tlabel="^label^"\n")
                end
            val _ = print "Evaluando expresión con value env:\n"
            fun not_extern (_,(Func {extern,...})) = not extern
              | not_extern (_,_) = true
            val _ = List.app pr1_val (List.filter not_extern (tabToList vt))
            val _ = print "Evaluando expresión con type env:\n"
            val _ = List.app pr1_type (tabToList tt)
            val (ir, ty) = seman' ee
        in
            print ("Resultado: (ir= "^(irToString ir)^", ty= "^(typeToString ty)^")\n") ;
            (ir, ty)
        end
  | UnitE _ => (Ex (Const 0), TUnit)
  | VarE (v,_) => varSeman vt tt v
  | NilE _ => (Ex (Const 0), TNil)
  | IntE (i,_) => (Ex (Const i), TInt)
  | StringE (s,_) =>
        let val ir = translate.stringExp s
        in (Ex ir, TString)
        end
  | CallE ({func,args}, ii) =>
      let val finfo =
              (case tabFind vt func of
                  NONE => semanError ii (func^": no existe")
                | SOME (Func info) => info
                | SOME _ => semanError ii (func^": no es función"))
          val formals = #formals finfo
          val ret     = #ret finfo
          val func_level = #level finfo
          val label   = #label finfo
          val extern = #extern finfo
          val seman_args = map seman' args
          val actual_types = map (#2) seman_args
          val _ = if length actual_types <> length formals
                    then semanError ii (func^": no coincide la cantidad de argumentos")
                    else ()
          val pairs = ListPair.zip (actual_types,formals)
          val matches = map (uncurry (typeMatch ii)) pairs
          val check =
           map (fn b =>
             ( if b then () else
             semanErrorNoThrow ii
               (func^": error de tipo en parámetro "^
               (makestring (~1))) ; b )) matches
          val allok = List.all (fn x => x) check
      in if not allok
         then raise SemanFail
         else let val args_e = map (unEx o #1) seman_args
                  val ir = trCall extern func_level (!curLevel) label args_e
              in (Ex ir, ret)
              end
      end
  | OpE ({left,oper,right}, ii) =>
    let fun arith oo l r =
          let val (li,lt) = seman' l
              val (ri,rt) = seman' r
          in if typeMatch ii lt TInt andalso typeMatch ii rt TInt
               then let val irop = case oo of
                                       PlusOp => Plus
                                     | MinusOp => Minus
                                     | MultOp => Mul
                                     | DivOp => Div
                                     | _ => semanError ii "internal error (arith)"
                    in (Ex (Binop (irop, unEx li, unEx ri)), TInt)
                    end
               else semanError ii "operandos no enteros"
          end
        fun eq oo l r =
          let val (li,lt) = seman' l
              val (ri,rt) = seman' r
          in if typeMatch ii lt rt (* matchean *)
                andalso not (lt = TNil andalso rt = TNil) (* no son ambos nil *)
                andalso not (lt = TUnit) (* ninguno es unit *)
               then if typeMatch ii lt TString
                    then let val irop = case oo of
                                            EqOp => Eq
                                          | NeqOp => Ne
                                          | _ => semanError ii "internal error (eq)"
                         in (Cx (fn (t,f) =>
                                    CJump (irop, Call (true, Name "_tiger_strcmp",
                                                        [unEx li, unEx ri]), Const 0, t, f)
                                ), TInt)
                         end

                    else let val irop = case oo of
                                            EqOp => Eq
                                          | NeqOp => Ne
                                          | _ => semanError ii "internal error (eq)"
                         in (Cx (fn (t,f) =>
                                    CJump (irop, unEx li, unEx ri, t, f)
                                ), TInt)
                         end
               else semanError ii "comparación de igualdad inválida"
          end
        fun ord oo l r =
          let val (li,lt) = seman' l
              val (ri,rt) = seman' r
          in if typeMatch ii lt rt andalso (typeMatch ii lt TInt orelse typeMatch ii lt TString)
               then if typeMatch ii lt TString
                    then let val irop = case oo of
                                            LtOp => Lt
                                          | GtOp => Gt
                                          | GeOp => Ge
                                          | LeOp => Le
                                          | _ => semanError ii "internal error (ord)"
                         in (Cx (fn (t,f) =>
                                    CJump (irop, Call (true, Name "_tiger_strcmp",
                                                        [unEx li, unEx ri]), Const 0, t, f)
                                ), TInt)
                         end

                    else let val irop = case oo of
                                            LtOp => Lt
                                          | GtOp => Gt
                                          | GeOp => Ge
                                          | LeOp => Le
                                          | _ => semanError ii "internal error (ord)"
                         in (Cx (fn (t,f) =>
                                    CJump (irop, unEx li, unEx ri, t, f)
                                ), TInt)
                         end
               else semanError ii "comparación de orden inválida"
          end
        val opertype = case oper of
                           PlusOp  => arith
                         | MinusOp => arith
                         | MultOp  => arith
                         | DivOp   => arith
                         | EqOp  => eq
                         | NeqOp => eq
                         | LtOp => ord
                         | GtOp => ord
                         | GeOp => ord
                         | LeOp => ord
    in opertype oper left right end
  | RecordE ({fields=flds, typ}, ii) =>
    let fun fldcmp ((n1,e1),(n2,e2)) = String.compare (n1,n2)
        val sorted_flds = Listsort.sort fldcmp flds
        val sorted_names = map (#1) sorted_flds
        val rectype = case tabFind tt typ of
                        SOME t => ( case tipoReal ii t of
                                      TRecord rrr => rrr
                                      | _ => semanError ii (typ^": no es de tipo record")
                                  )
                        | NONE => semanError ii (typ^": no existe el tipo")
        val formal_types = #1 rectype

        val flds_seman = map (fn (n,e) => (n, seman' e)) sorted_flds
        val flds_expr = map (fn (_,(e,_)) => unEx e) flds_seman

        fun check_flds [] [] = true
          | check_flds [] ((nf,_)::_) = ( semanErrorNoThrow ii (typ^": falta el campo "^nf) ; false)
          | check_flds ((na,_)::_) [] = ( semanErrorNoThrow ii (typ^": el campo "^na^" no pertenece al tipo") ; false )
          | check_flds ((na,(_,ta))::aa) ((nf,tf)::fs) =
             if na < nf
             then semanError ii (typ^": el campo "^na^" no pertenece al tipo")
             else if na > nf
             then semanError ii (typ^": falta el campo "^nf)
             else (* na = nf *)
              case typeMatch ii ta tf of
               true => check_flds aa fs
               | false => ( semanErrorNoThrow ii (typ^": el campo "^na^" no coincide con su tipo formal") ;
                          check_flds aa fs ;
                          false )
    in
      if check_flds flds_seman formal_types
      then (Ex (Call (true, Name "__mk_record", (Const (length flds_expr))::flds_expr)), TRecord rectype)
      else raise SemanFail
    end
  | SeqE (es, _) =>
    let val semans = map seman' es
        val (lastr, lastty) = List.last semans
        val init = init semans
        val stms = SEQ (map (unNx o (#1)) init)
    in (Ex (eseq (stms, unEx lastr)), lastty) end
  | AssignE ({l,r}, ii) =>
    let val (li,lt) = varSeman vt tt l
        val (ri,rt) = seman' r
    in if typeMatch ii lt rt andalso lt <> TIntRO
         then (Nx (Move (unEx li, unEx ri)), TUnit)
         else semanError ii "asignación inválida"
    end
  | IfE ({test,th,el=SOME el}, ii) =>
    let val (testi, testtip) = seman' test
    in if typeMatch ii testtip TInt
         then let val (li, lt) = seman' th
                  val (ri, rt) = seman' el
               in if typeMatch ii lt rt
                     then let val tlab = newlabel ()
                              val elab = newlabel ()
                              val join = newlabel ()
                              val t = newtemp ()
                              val prep = SEQ [unCx testi (tlab, elab),
                                              Label tlab,
                                              Move (Temp t, unEx li),
                                              Jump (Name join, [join]),
                                              Label elab,
                                              Move (Temp t, unEx ri),
                                              Jump (Name join, [join]),
                                              Label join
                                             ]
                          in  (Ex (eseq (prep, Temp t)), lt)
                          end
                     else semanError ii "las ramas del if tipan distinto"
               end
         else
           semanError ii "el test del if no es de tipo entero"
    end
  | IfE ({test,th, el=NONE}, ii) =>
    let val (testi, testtip) = seman' test
    in if typeMatch ii testtip TInt
         then let val (li, lt) = seman' th
               in if typeMatch ii lt TUnit
                     then let val tlab = newlabel ()
                              val join = newlabel ()
                          in (Nx (SEQ [unCx testi (tlab, join),
                                       Label tlab,
                                       unNx li,
                                       Label join
                                      ]
                                 ), TUnit)
                          end
                     else semanError ii "la rama del if imperativo no tipa a unit"
               end
         else
           semanError ii "el test del if no es de tipo entero"
    end
  | WhileE ({test,body}, ii) =>
    let val (testi, testt) = seman' test
        val breaklabel = newlabel ()
        val _ = pushLoopLabel breaklabel
        val (bodyi, bodyt) = seman' body
        val _ = popLoopLabel ()
      in if typeMatch ii testt TInt then
           if typeMatch ii bodyt TUnit
             then let val start = newlabel ()
                      val cond = newlabel ()
                      val loopir = SEQ [Label cond,
                                        unCx testi (start, breaklabel),
                                        Label start,
                                        unNx bodyi,
                                        Jump (Name cond, [cond]),
                                        Label breaklabel
                                       ]
                  in (Nx loopir , TUnit)
                  end
           else
             semanError ii "el cuerpo del while no tipa a unit"
         else
           semanError ii "el test del while no tipa a entero"
      end
  | ForE ({index,lo,hi,body,escape}, ii) =>
    let val (loi, lot) = seman' lo
        val (hii, hit) = seman' hi
        val _ = if not (typeMatch ii lot TInt) then semanError ii "el 'low' del for no tipa a int" else ()
        val _ = if not (typeMatch ii hit TInt) then semanError ii "el 'high' del for no tipa a int" else ()
        val newvt = tabCopy vt
        val index_acc = allocLocal (!curLevel) (!escape)
        val _ = tabReplace newvt (index, Var {ty=TIntRO, acc=index_acc, level= !curLevel})
        val breaklabel = newlabel ()
        val _ = pushLoopLabel breaklabel
        val (bodyi, bodyt) = seman newvt tt body
        val _ = popLoopLabel ()
        val lbl_body = newlabel ()
        val lbl_t1 = newlabel ()
        val lbl_inc = newlabel ()
        val idx_expr = simpleVar index_acc (!curLevel)
        val lo_r = Temp (newtemp ())
        val hi_r = Temp (newtemp ())
     in if typeMatch ii bodyt TUnit
          then let val loopir = SEQ [Move (lo_r, unEx loi),
                                     Move (hi_r, unEx hii),
                                     CJump (Gt, lo_r, hi_r, breaklabel, lbl_t1),
                                     Label lbl_t1,
                                     Move (idx_expr, lo_r),
                                     Label lbl_body,
                                     unNx bodyi,
                                     CJump (Eq, idx_expr, hi_r, breaklabel, lbl_inc),
                                     Label lbl_inc,
                                     Move (idx_expr, Binop (Plus, Const 1, idx_expr)),
                                     Jump (Name lbl_body, [lbl_body]),
                                     Label breaklabel
                                     ]
               in (Nx loopir, TUnit)
               end
          else semanError ii "el cuerpo del for no tipa a unit"
     end
  | LetE ({decs, body},_) =>
    let fun proc_decl (dec,(exps, vt', tt')) =
            let val (e, vt', tt') = declSeman vt' tt' dec
            in (e::exps, vt', tt') end
        val (exps, newvt, newtt) = foldl proc_decl ([], vt,tt) decs
        val exps' = rev exps
        val (body_ir, body_t) = seman newvt newtt body
    in (Ex (eseq (SEQ exps', unEx body_ir)), body_t)
    end
  | BreakE ii => if !labelStack = []
                 then semanError ii "break fuera de bucle"
                 else let val label = peekLoopLabel ()
                       in (Nx (Jump (Name label, [label])), TUnit) end
  | ArrayE ({typ,size,init}, ii) =>
    let val (elemt, uq) = case tabFind tt typ of
                            SOME t => ( case tipoReal ii t of
                                          TArray (t,uq) => (t,uq)
                                          | _ => semanError ii (typ^": no es de tipo array")
                                      )
                            | NONE => semanError ii ("no existe el tipo "^typ)
        val (init_e, initt) = seman' init
        val (size_e, sizet) = seman' size
        in if typeMatch ii elemt initt
               then if typeMatch ii sizet TInt
                    then (Ex (Call (true, Name "__mk_array", [unEx init_e, unEx size_e])),
                            TArray (elemt, uq))
                    else semanError ii "el tamaño del array no tipa a entero"
               else semanError ii "la inicialización del array no tipa al tipo del array"
        end
end
and varSeman vt tt (SimpleVar (s, ii)) =
       (case tabFind vt s of
                  SOME (Var {ty, acc, level}) =>
                        (Ex (simpleVar acc (!curLevel)), ty)
                | SOME _ =>
                        semanError ii (s^": no es variable")
                | NONE =>
                        semanError ii (s^": variable no definida")
       )
  | varSeman vt tt (IndexVar (arr, idx, ii)) =
        let val (arre, arrt) = varSeman vt tt arr
            val elemt = case tipoReal ii arrt of
                          TArray (e, _) => e
                          | _ => semanError ii ("subscript a elemento no array")
            val (idxe, idxt) = seman vt tt idx
            val _ = if typeMatch ii idxt TInt then ()
                        else semanError ii "subscript no es de tipo entero"

        in (indexVar (unEx arre) (unEx idxe), elemt)
       end

  | varSeman vt tt (FieldVar (record,fld, ii)) =
      let val (recorde, recordt) = varSeman vt tt record
          val flds = case tipoReal ii recordt of
                       TRecord (flds,_) => flds
                       | _ => semanError ii ("field a elemento no record")
          val fldt = case List.filter (fn (s,_) => s = fld) flds of
                       [] => semanError ii (fld^": no existe el campo dentro del tipo del record")
                       | [(_,typ)] => typ
                       | _ => semanError ii "error interno! (1)"
          val idx = valOf (find_idx fld flds)
      in (indexVar (unEx recorde) (Const idx), fldt) end

and declSeman vt tt (VarDecl ({name,escape,typ,init}, ii)) =
      let val (initir, initt) = seman vt tt init
          val formaltype = case typ of
                             SOME typename => ( case tabFind tt typename of
                                                  SOME t => tipoReal ii t
                                                  | NONE => semanError ii (typename^": no existe el tipo (en inicialización)")
                                              )
                             | NONE => initt
      in if initt = TNil andalso typ = NONE
         then semanError ii "no se puede inicializar a nil sin explicitar el tipo de record"
         else if typeMatch ii formaltype initt
              then let val newvt = tabCopy vt
                       val varacc = allocLocal (!curLevel) (!escape)
                       val _ = tabReplace newvt (name, Var {ty=formaltype, acc=varacc, level= !curLevel})
                   in (Move (simpleVar varacc (!curLevel), unEx initir),
                           newvt, tt) end
              else semanError ii "tipo inferido y declarado difieren"
      end
  | declSeman vt tt (FuncDecl fun_dec_list) =
      let val newvt = tabCopy vt
          val _ = if checkDups (map (#name o #1) fun_dec_list)
                   then semanError (#2 (hd fun_dec_list)) "funciones duplicadas en el batch"
                   else ()
          fun add_one (fd, ii) =
              let fun type_lookup typ =
                       case tabFind tt typ of
                         SOME t => tipoReal ii t
                         | NONE => semanError ii (typ^": no existe el tipo")
                  val _ = if checkDups (map (#name) (#params fd))
                             then semanError ii "Argumentos duplicados en declaración de función"
                             else ()
                  val argstypes = map (type_lookup o #typ) (#params fd)
                  val rettype = case #result fd of
                                  SOME t => type_lookup t
                                  | NONE => TUnit

                  val argescape = map ((!) o (#escape)) (#params fd)
                  val label = mklabel (#name fd, infoline ii)

                  val level = newLevel { parent=(!curLevel), name=label,
                                         formals=argescape }

                  val functype  =  Func { formals=argstypes,
                                          ret = rettype,
                                          extern = false,
                                          label = label,
                                          level = level }
              in
                  tabReplace newvt (#name fd, functype)
              end
          fun proc_one (fd, ii) =
              let val localvt = tabCopy newvt
                  val finfo = ( case tabTake newvt (#name fd) of
                                  Func x => x
                                  | _ => raise Fail ""
                              ) handle _ => semanError ii "error interno muy podrido"
                  val label = #label finfo
                  val level = #level finfo

                  val oldLevel = !curLevel
                  fun argtype arg = case tabFind tt (#typ arg) of
                                      SOME t => tipoReal ii t
                                      | NONE => semanError ii ((#typ arg)^": no existe el tipo.")

                  val _ = curLevel := level

                  fun arg2env (arg, acc) = let
                                    val arg_name = #name arg
                                    val arg_type = argtype arg
                                  in
                                    tabReplace localvt
                                        (arg_name, Var { ty    = arg_type,
                                                         acc   = acc,
                                                         level = !curLevel
                                                       } )
                                  end

                  val _ = List.app arg2env (ListPair.zip (#params fd, formals level))
                  val (bodyir, bodyt) = seman localvt tt (#body fd)

                  val _ = curLevel := oldLevel

                  val rettype = case tabTake newvt (#name fd) of
                                  Func {ret,...} => ret
                                  | _ => semanError ii "error interno re podrido"
              in if typeMatch ii bodyt rettype
                  then funcDecl level bodyir
                  else semanError ii "el cuerpo de la función no tipa al retorno de la función"
              end
      in
         ( List.app add_one fun_dec_list ;
           List.app proc_one fun_dec_list ;
           (Skip, newvt, tt) )
      end
  | declSeman vt tt (TypeDecl typ_dec_list) =
      let val newtt = tabCopy tt
          val trucho_ii = #2 (hd typ_dec_list)
          (* deteccion de ciclos *)
          fun find_empty_types () =
              let fun dep ({name,ty}, ii) =
                      case ty of
                        NameTy t2 =>
                           ( case List.filter (fn ({name,ty},_) => name = t2) typ_dec_list of
                               [] => []
                               | _ => [(name,t2)]
                           )
                        | ArrayTy t2 =>
                           ( case List.filter (fn ({name,ty},_) => name = t2) typ_dec_list of
                               [] => []
                               | _ => [(name,t2)]
                           )
                        | RecordTy _ => []
              val dep_pairs = List.concat (map dep typ_dec_list)
              val typ_list = map (fn (td,ii) => #name td) typ_dec_list
           in case topSort dep_pairs typ_list of
                 OK _ => NONE
                 | CICLE t => SOME t
           end

          val circular_fix = ref []
          fun check_dups () = let val namelist =
                                    map (fn ({name,ty},_) => name) typ_dec_list
                               in if checkDups namelist
                                    then semanError trucho_ii "tipos duplicados en un mismo batch"
                                    else ()
                              end
          fun add_one ({name,ty},_) = tabReplace newtt (name, TReference (ref NONE))
          fun proc_one ({name,ty},ii) =
              let val rr = case tabFind newtt name of
                             SOME (TReference rr) => rr
                             | _ => semanError ii "error interno (5)"
              in
                case ty of
                  NameTy s =>
                      ( case tabFind newtt s of
                          SOME t => rr := SOME t
                          | NONE => semanError ii (s^": no existe el tipo (sinónimo)")
                      )
                  | ArrayTy s =>
                      ( case tabFind newtt s of
                          SOME t => rr := SOME (TArray (t, ref ()))
                          | NONE => semanError ii (s^": no existe el tipo (array)")
                      )
                  | RecordTy flds =>
                      let fun fldcmp ({name=n1,...},{name=n2,...}) = String.compare (n1,n2)
                          val sorted_flds = Listsort.sort fldcmp flds
                          fun get_type {name,typ} =
                            case tabFind newtt typ of
                                  SOME t => (name,t)
                                  | NONE => semanError ii (typ^": no existe el tipo (record)")
                      in rr := SOME (TRecord (map get_type sorted_flds, ref ())) end
              end
      in
          ( check_dups () ;
            ( case find_empty_types () of
                NONE => ()
                | SOME (h::t) => let val cicle_str = foldl (fn (n, a) => n^", "^a) h t
                                 in semanError trucho_ii (cicle_str^": tipos inhabitables")
                                 end
                | SOME _ => semanError trucho_ii "error interno 6" ) ;
            List.app add_one typ_dec_list ;
            List.app proc_one typ_dec_list ;
            (Skip, vt, newtt) )
      end

fun semantics tree =
    let fun wrap exp =
        LetE ({ decs= [ FuncDecl [({ name= "_tigermain",
                                     params= [],
                                     result= NONE,
                                     body= SeqE ([exp, CallE ({func="exit", args=[IntE (0,fakeinfo)]},fakeinfo)], fakeinfo)
                                   }, fakeinfo)]
                      ], body= UnitE fakeinfo
              }, fakeinfo)
     in seman init_venv init_tenv (wrap tree) ;
        if !verbose then print "Semantics: finalizado ok\n" else ()
    end

end (* struct *)
