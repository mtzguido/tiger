structure semantics :> semantics =
struct

open ast hash types common topsort

exception SemanFail

datatype IR = SCAF
datatype EnvEntry =
    Var of TigerType
  | Func of {formals:TigerType list, ret:TigerType, extern:bool, label:string}

val labelStack = ref []

fun pushLoopLabel () = labelStack := ()::(!labelStack)
fun popLoopLabel  () = labelStack := tl (!labelStack)
fun peekLoopLabel () = hd (!labelStack)

val init_venv : (symbol, EnvEntry) Tabla  = tabNew ()
val init_tenv : (symbol, TigerType) Tabla = tabNew ()

val _ = tabInsertList init_tenv [("int", TInt), ("string", TString)]
val _ = tabInsertList init_venv [("x", Var TIntRO)]

fun elem x [] = false
  | elem x (e::es) = x = e orelse elem x es

fun checkDups [] = false
  | checkDups (e::es) = if elem e es then true else checkDups es

fun semanError info s = ( print ("Semantic: Error en "^(info2str info)^".\nSemantic: "^s^".\n") ;
                          raise SemanFail )
fun semanErrorNoThrow info s = print ("Semantic: Error en "^(info2str info)^".\nSemantic: "^s^".\n")

fun uncurry f (x,y) = f x y

fun tipoReal t = case t of
    TReference r => ( case !r of
                        SOME tt => tipoReal tt
                        | NONE => raise Fail "errrrrrrrr" )
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
              NONE => semanError ii "error interno: referencia inválida"
              | SOME t => typeMatch ii tt t
          )
  | (TReference tref, tt)
       => ( case !tref of
              NONE => semanError ii "error interno: referencia inválida"
              | SOME t => typeMatch ii t tt
          )
  | (_,_) => false

fun seman vt tt exp =
 let fun seman' e = seman vt tt e
 in case exp of
    UnitE _ => (SCAF, TUnit)
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
                    then semanError ii (func^": no conincide la cantidad de argumentos")
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
                        SOME (TRecord rrr) => rrr
                        | SOME _ => semanError ii (typ^": no es un tipo record")
                        | NONE => semanError ii (typ^": no existe el tipo")
        val formal_types = #1 rectype
        val actual_types = map (fn (n,e) => (n, #2 (seman' e))) sorted_flds
        fun check_flds [] [] = true
          | check_flds [] ((nf,_)::_) = ( semanErrorNoThrow ii (typ^": falta el campo "^nf) ; false)
          | check_flds ((na,_)::_) [] = ( semanErrorNoThrow ii (typ^": el campo "^na^" no pertenece al tipo") ; false )
          | check_flds ((na,ta)::aa) ((nf,tf)::fs) = 
             if na < nf
             then semanError ii (typ^": el campo "^na^" no pertenece al tipo") (* esto esta mal, puede ser na o nf *)
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
    let val tt = case tabFind tt typ of
                   SOME t => tipoReal t
                   | NONE => semanError ii ("no existe el tipo "^typ)
        val (_,initt) = seman' init
        in if typeMatch ii tt initt
               then (SCAF, TArray (tt, ref ()))
               else semanError ii "la inicialización del array no tipa al tipo del array"
        end
end
and varSeman vt tt (SimpleVar (s,ii)) = ( case tabFind vt s of
                                           SOME (Var t) => (SCAF, t)
                                           | NONE => semanError ii (s^": variable no definida")
                                           | _ => semanError ii (s^": no es variable") )
  | varSeman vt tt (IndexVar (arr,idx, ii)) =
        let val elemt = case varSeman vt tt arr of
                          (_, TArray (e,_)) => e
                          | _ => semanError ii ("subscript a elementno no array")
            val (_,idxt) = seman vt tt idx
        in if typeMatch ii idxt TInt
             then (SCAF, elemt)
             else semanError ii "subscript no es de tipo entero"
       end
  | varSeman vt tt (FieldVar (record,fld, ii)) =
      let val flds = case varSeman vt tt record of
                       (_, TRecord (flds,_)) => flds
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
                                                  SOME t => tipoReal t
                                                  | NONE => semanError ii (typename^": no existe el tipo (en inicialización)")
                                              )
                             | NONE => initt
      in if typeMatch ii formaltype initt
           then let val newvt = tabCopy vt
                    val _ = tabReplace newvt (name, Var formaltype)
                in (newvt, tt) end
           else semanError ii "tipo inferido y declarado difieren"
      end
  | declSeman vt tt (FuncDecl fun_dec_list) =
      let val newvt = tabCopy vt
          val _ = if checkDups (map (#name o #1) fun_dec_list) then raise Fail "funciones duplicadas en batch" else ()
          fun add_one (fd, info) =
              let fun type_lookup typ = case tabFind tt typ of
                                          SOME t => tipoReal t
                                          | NONE => raise Fail "tipo no existente en param"
                  val argstypes = map (type_lookup o #typ) (#params fd)
                  val rettype = case #result fd of
                                  SOME t => ( case tabFind tt t of
                                                SOME ttt => tipoReal ttt
                                                | NONE => raise Fail "asdasd123" )
                                  | NONE => TUnit
                  val functype = Func{formals=argstypes,ret=rettype,extern=false,label="??"}
              in
                  tabReplace newvt (#name fd, functype)
              end
          fun proc_one (fd, ii) =
              let val localvt = tabCopy newvt
                  fun argtype arg = case tabFind tt (#typ arg) of
                                      SOME t => tipoReal t
                                      | NONE => raise Fail "no existe tipo de argumento (no deberia ocurrir)"
                  fun arg2env a = tabReplace localvt (#name a, Var (argtype a))
                  val _ = map arg2env (#params fd)
                  val (bodyir, bodyt) = seman localvt tt (#body fd)
                  val rettype = case tabTake newvt (#name fd) of
                                  Func {ret,...} => ret
                                  | _ => raise Fail "imposible"
              in if typeMatch ii bodyt rettype
                  then  ()
                  else raise Fail "formal return type and body type differ"
              end
      in
         ( List.app add_one fun_dec_list ;
           map proc_one fun_dec_list ;
           (newvt, tt) )
      end
  | declSeman vt tt (TypeDecl typ_dec_list) =
      let val newtt = tabCopy tt
          fun dep ({name,ty}, ii) =
              case ty of
                  NameTy t2 => ( case List.filter (fn ({name,ty},_) => name = t2) typ_dec_list of
                                   [] => []
                                   | _ => [(name,t2)] )
                  | ArrayTy t2 => ( case List.filter (fn ({name,ty},_) => name = t2) typ_dec_list of
                                    [] => []
                                    | _ => [(name,t2)] )
                  | RecordTy _ => []
          val dep_pairs = List.concat (map dep typ_dec_list)
          val _ = List.app (fn (a,b) => print ("dep: ("^a^", "^b^")\n")) dep_pairs
          val typ_list = map (fn td => #name (#1 td)) typ_dec_list
          val _ = List.app (fn t => print ("tipo: "^t^"\n")) typ_list
          val ordered_types = topSort dep_pairs typ_list
          val _ = List.app (fn t => print ("TIPO: "^t^"\n")) ordered_types
          val circular_fix = ref []
          fun proc_one name =
              let val ty = case List.filter (fn (td,_) => (#name td) = name) typ_dec_list of
                             x::[] => #ty (#1 x)
                             | _ => raise Fail "tipo usado en type batch y no def... arreglar y detectar antes de topsort (o no?) tambien cheququar duplicados"
                  val real_type =
                      case ty of
                         NameTy s => ( case tabFind newtt s of
                                         SOME t => t
                                         | NONE => ( print "!!!!!!!1" ; TReference (ref NONE) )
                                     )
                         | ArrayTy s => ( case tabFind newtt s of
                                            SOME t => TArray (t, ref ())
                                            | NONE => ( print "%&!$@|!!" ; TArray (TReference (ref NONE), ref ()) )
                                        )
                         | RecordTy flds =>
                                let fun get_type {name,typ} = case tabFind newtt typ of
                                            SOME t => (name,t)
                                            | NONE => let val rr = ref NONE
                                                      in ( circular_fix := (rr, typ)::(!circular_fix) ;
                                                           (name, TReference (ref NONE)) ) end
                                in TRecord (map get_type flds, ref ()) end
               in ( print ("agregado tipo: "^name^"\n");
                    tabReplace newtt (name, real_type) ) end
          fun fix_one (r, n) = ( print ("buscando tipo "^n^"\n") ;
                                 r := SOME (tabTake newtt n) )
      in
          ( List.app proc_one ordered_types ;
            print "1\n" ;
            List.app fix_one (!circular_fix) ;
            print "2\n" ;
            (vt, newtt) )
      end handle Ciclo => raise Fail "Ciclo en delaracion de tipos"

fun semantics tree = ( seman init_venv init_tenv tree ;
                       if !verbose then print "Semantics: finalizado ok\n" else () )

end (* struct *)
