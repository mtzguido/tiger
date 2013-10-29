structure semantics :> semantics =
struct

open ast hash types common topsort ir

exception SemanFail

datatype EnvEntry =
    Var of TigerType
  | Func of { formals : TigerType list,
              ret : TigerType,
              extern : bool,
              label : string,
              level : int }

val labelStack = ref []
val curLevel = ref 0

fun pushLoopLabel () = labelStack := ()::(!labelStack)
fun popLoopLabel  () = labelStack := tl (!labelStack)
fun peekLoopLabel () = hd (!labelStack)

val last_fun_label = ref 0

fun mklabel (name, lineno) =
    (name^"_"^(makestring lineno)^"_"^(makestring (!last_fun_label)))
    before last_fun_label := !last_fun_label + 1

val init_venv : (symbol, EnvEntry) Tabla  = tabNew ()
val init_tenv : (symbol, TigerType) Tabla = tabNew ()

val _ = tabInsertList init_tenv [("int", TInt), ("string", TString)]
val _ = tabInsertList init_venv
    [("chr",       Func{formals=[TInt], ret=TString, extern=true, label="_tiger_chr", level=0}),
     ("concat",    Func{formals=[TString,TString], ret=TString, extern=true, label="_tiger_concat", level=0}),
     ("exit",      Func{formals=[TInt], ret=TUnit, extern=true, label="_tiger_exit", level=0}),
     ("flush",     Func{formals=[], ret=TUnit, extern=true, label="_tiger_flush", level=0}),
     ("getchar",   Func{formals=[], ret=TString, extern=true, label="_tiger_getchar", level=0}),
     ("not",       Func{formals=[TInt], ret=TInt, extern=true, label="_tiger_not", level=0}),
     ("ord",       Func{formals=[TString], ret=TInt, extern=true, label="_tiger_ord", level=0}),
     ("print",     Func{formals=[TString], ret=TUnit, extern=true, label="_tiger_print", level=0}),
     ("print_err", Func{formals=[TString], ret=TUnit, extern=true, label="_tiger_print_err", level=0}),
     ("print_int", Func{formals=[TInt], ret=TUnit, extern=true, label="_tiger_print_int", level=0}),
     ("size",      Func{formals=[TString], ret=TInt, extern=true, label="_tiger_size", level=0}),
     ("strcmp",    Func{formals=[TString,TString], ret=TInt, extern=true, label="_tiger_strcmp", level=0}),
     ("streq",     Func{formals=[TString,TString], ret=TInt, extern=true, label="_tiger_streq", level=0}),
     ("substring", Func{formals=[TString,TInt,TInt], ret=TString, extern=true, label="_tiger_substring", level=0})
    ]

fun elem x [] = false
  | elem x (e::es) = x = e orelse elem x es

fun checkDups [] = false
  | checkDups (e::es) = if elem e es then true else checkDups es

fun semanError info s = ( print ("Semantics: Error en "^(info2str info)^".\nSemantics: "^s^".\n") ;
                          raise SemanFail )
fun semanErrorNoThrow info s = print ("Semantics: Error en "^(info2str info)^".\nSemantics: "^s^".\n")

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
            fun pr1_val (n, Var t) = 
                print ("\t("^n^", Var "^(typeToString t)^")\n")
              | pr1_val (n, Func {formals,ret,extern,label,level}) =
                let in
                    print ("\t("^n^", Func\n") ;
                    print ("\t\targs=\n") ;
                    List.app (fn t => print ("\t\t\t"^(typeToString t)^"\n")) formals ;
                    print ("\t\treturn=bleh\n") ;
                    print ("\t\t\t"^(typeToString ret)^"\n") ;
                    print ("\t\textern="^(if extern then "yes" else "no")^"\n") ;
                    print ("\t\tlabel="^label^"\n") ;
                    print ("\t\tlevel="^(makestring level)^"\n")
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
  | UnitE _ => (SCAF, TUnit)
  | VarE (v,_) => varSeman vt tt v
  | NilE _ => (SCAF, TNil)
  | IntE (i,_) => (SCAF, TInt)
  | StringE (s,_) => (SCAF, TString)
  | CallE ({func,args}, ii) =>
      let val (formals, ret) =
            ( case tabFind vt func of
                  NONE => semanError ii (func^": no existe")
                | SOME (Func {formals,ret,...}) => (formals,ret)
                | SOME _ => semanError ii (func^": no es función") )
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
      in if allok then (SCAF, ret) else raise SemanFail end
  | OpE ({left,oper,right}, ii) =>
    let fun arith oo l r =
          let val (_,lt) = seman' l
              val (_,rt) = seman' r
          in if typeMatch ii lt TInt andalso typeMatch ii rt TInt
               then (SCAF, TInt)
               else semanError ii "operandos no enteros"
          end
        fun eq oo l r =
          let val (_,lt) = seman' l
              val (_,rt) = seman' r
          in if typeMatch ii lt rt (* matchean *)
                andalso not (lt = TNil andalso rt = TNil) (* no son ambos nil *)
                andalso not (lt = TUnit) (* ninguno es unit *)
               then (SCAF, TInt)
               else semanError ii "comparación de igualdad inválida"
          end
        fun ord oo l r =
          let val (_,lt) = seman' l
              val (_,rt) = seman' r
          in if typeMatch ii lt rt andalso (typeMatch ii lt TInt orelse typeMatch ii lt TString)
               then (SCAF, TInt)
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
        val actual_types = map (fn (n,e) => (n, #2 (seman' e))) sorted_flds
        fun check_flds [] [] = true
          | check_flds [] ((nf,_)::_) = ( semanErrorNoThrow ii (typ^": falta el campo "^nf) ; false)
          | check_flds ((na,_)::_) [] = ( semanErrorNoThrow ii (typ^": el campo "^na^" no pertenece al tipo") ; false )
          | check_flds ((na,ta)::aa) ((nf,tf)::fs) =
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
      if check_flds actual_types formal_types
      then (SCAF, TRecord rectype) else raise SemanFail
    end
  | SeqE (es, _) =>
    let val semans = map seman' es
        val last = List.last semans
    in (SCAF, #2 last) end
  | AssignE ({l,r}, ii) =>
    let val (li,lt) = varSeman vt tt l
        val (ri,rt) = seman' r
    in if typeMatch ii lt rt andalso lt <> TIntRO
         then (SCAF, TUnit)
         else semanError ii "asignación inválida"
    end
  | IfE ({test,th,el=SOME el}, ii) =>
    let val (_, testtip) = seman' test
    in if typeMatch ii testtip TInt
         then let val (_, lt) = seman' th
                  val (_, rt) = seman' el
               in if typeMatch ii lt rt
                     then (SCAF, lt)
                     else semanError ii "las ramas del if tipan distinto"
               end
         else
           raise semanError ii "el test del if no es de tipo entero"
    end
  | IfE ({test,th, el=NONE}, ii) =>
    let val (_, testtip) = seman' test
    in if typeMatch ii testtip TInt
         then let val (_, lt) = seman' th
               in if typeMatch ii lt TUnit
                     then (SCAF, TUnit)
                     else semanError ii "la rama del if imperativo no tipa a unit"
               end
         else
           raise semanError ii "el test del if no es de tipo entero"
    end
  | WhileE ({test,body}, ii) =>
    let val (_, testt) = seman' test
        val _ = pushLoopLabel ()
        val (_, bodyt) = seman' body
        val _ = popLoopLabel ()
      in if typeMatch ii testt TInt then
           if typeMatch ii bodyt TUnit
             then (SCAF, TUnit)
           else
             semanError ii "el cuerpo del while no tipa a unit"
         else
           semanError ii "el test del while no tipa a entero"
      end
  | ForE ({index,lo,hi,body,...}, ii) =>
    let val (_,lot) = seman' lo
        val (_,hit) = seman' hi
        val _ = if not (typeMatch ii lot TInt) then semanError ii "el 'low' del for no tipa a int" else ()
        val _ = if not (typeMatch ii hit TInt) then semanError ii "el 'high' del for no tipa a int" else ()
        val newvt = tabCopy vt
        val _ = tabReplace newvt (index, Var TIntRO)
        val _ = pushLoopLabel ()
        val (bodyir, bodyt) = seman newvt tt body
        val _ = popLoopLabel ()
     in if typeMatch ii bodyt TUnit
          then (SCAF, TUnit)
          else semanError ii "el cuerpo del for no tipa a unit"
     end
  | LetE ({decs, body},_) =>
    let fun proc_decl (dec,(vt', tt')) = declSeman vt' tt' dec
        val (newvt, newtt) = foldl proc_decl (vt,tt) decs
    in seman newvt newtt body
    end
  | BreakE ii => if !labelStack = [] then semanError ii "break fuera de bucle" else (SCAF, TUnit)
  | ArrayE ({typ,size,init}, ii) =>
    let val (elemt, uq) = case tabFind tt typ of
                            SOME t => ( case tipoReal ii t of
                                          TArray (t,uq) => (t,uq)
                                          | _ => semanError ii (typ^": no es de tipo array")
                                      )
                            | NONE => semanError ii ("no existe el tipo "^typ)
        val (_,initt) = seman' init
        val (_,sizet) = seman' size
        in if typeMatch ii elemt initt
               then if typeMatch ii sizet TInt
                    then (SCAF, TArray (elemt, uq))
                    else semanError ii "el tamaño del array no tipa a entero"
               else semanError ii "la inicialización del array no tipa al tipo del array"
        end
end
and varSeman vt tt (SimpleVar (s,ii)) = ( case tabFind vt s of
                                           SOME (Var t) => (SCAF, t)
                                           | NONE => semanError ii (s^": variable no definida")
                                           | _ => semanError ii (s^": no es variable") )
  | varSeman vt tt (IndexVar (arr,idx, ii)) =
        let val (_, arrt) = varSeman vt tt arr
            val elemt = case tipoReal ii arrt of
                          TArray (e,_) => e
                          | _ => semanError ii ("subscript a elementno no array")
            val (_,idxt) = seman vt tt idx
        in if typeMatch ii idxt TInt
             then (SCAF, elemt)
             else semanError ii "subscript no es de tipo entero"
       end
  | varSeman vt tt (FieldVar (record,fld, ii)) =
      let val (_, recordt) = varSeman vt tt record
          val flds = case tipoReal ii recordt of
                       TRecord (flds,_) => flds
                       | _ => semanError ii ("field a elemento no record")
          val fldt = case List.filter (fn (s,_) => s = fld) flds of
                       [] => semanError ii (fld^": no existe el campo dentro del tipo del record")
                       | [(_,typ)] => typ
                       | _ => semanError ii "error interno! (1)"
      in (SCAF, fldt) end
and declSeman vt tt (VarDecl ({name,escape,typ,init}, ii)) =
      let val (initir,initt) = seman vt tt init
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
                       val _ = tabReplace newvt (name, Var formaltype)
                   in (newvt, tt) end
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
                  val functype  =  Func { formals=argstypes,
                                        ret = rettype,
                                        extern = false,
                                        label = mklabel (#name fd, infoline ii),
                                        level = !curLevel }
              in
                  tabReplace newvt (#name fd, functype)
              end
          fun proc_one (fd, ii) =
              let val localvt = tabCopy newvt
                  fun argtype arg = case tabFind tt (#typ arg) of
                                      SOME t => tipoReal ii t
                                      | NONE => semanError ii ((#typ arg)^": no existe el tipo.")
                  fun arg2env a = tabReplace localvt (#name a, Var (argtype a))
                  val         _ = List.app arg2env (#params fd)
                  val (bodyir, bodyt) = seman localvt tt (#body fd)
                  val rettype = case tabTake newvt (#name fd) of
                                  Func {ret,...} => ret
                                  | _ => semanError ii "error interno re podrido"
              in if typeMatch ii bodyt rettype
                  then  ()
                  else semanError ii "el cuerpo de la función no tipa al retorno de la función"
              end
      in
         ( List.app add_one fun_dec_list ;
           curLevel := !curLevel + 1 ;
           List.app proc_one fun_dec_list ;
           curLevel := !curLevel - 1 ;
           (newvt, tt) )
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
            (vt, newtt) )
      end

fun semantics tree = 
    let fun wrap exp = 
        LetE ({ decs= [FuncDecl [({ name= "_tigermain",
                                                   params= [],
                                                   result= NONE,
                                                   body= SeqE ([exp, CallE ({func="exit", args=[IntE (0,fakeinfo)]},fakeinfo)], fakeinfo)
                                                 }, fakeinfo)]
                                     ], body= UnitE fakeinfo}, fakeinfo)
     in seman init_venv init_tenv (wrap tree) ;
        if !verbose then print "Semantics: finalizado ok\n" else ()
    end

end (* struct *)
