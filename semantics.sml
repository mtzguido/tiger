structure semantics :> semantics =
struct

open ast hash types common topsort

datatype IR = SCAF
datatype EnvEntry =
    Var of TigerType
  | Func of {formals:TigerType list, ret:TigerType, extern:bool, label:string}

val labelStack = ref []

fun pushLoopLabel () = labelStack := ()::(!labelStack)
fun popLoopLabel () = labelStack := tl (!labelStack)

val init_venv : (symbol, EnvEntry) Tabla  = tabNew ()
val init_tenv : (symbol, TigerType) Tabla = tabNew ()

val _ = tabInsertList init_tenv [("int", TInt), ("string", TString)]
val _ = tabInsertList init_venv [("x", Var TIntRO)]

fun elem x [] = false
  | elem x (e::es) = x = e orelse elem x es

fun checkDups [] = false
  | checkDups (e::es) = if elem e es then true else checkDups es


fun uncurry f (x,y) = f x y

fun tipoReal t = case t of
    TReference r => ( case !r of
                        SOME tt => tipoReal tt
                        | NONE => raise Fail "errrrrrrrr" )
  | otracosa => otracosa

fun typeMatch t1 t2 = case (t1, t2) of
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
              NONE => raise Fail "sinónimo inválido"
              | SOME t => typeMatch tt t
          )
  | (TReference tref, tt)
       => ( case !tref of
              NONE => raise Fail "sinónimo inválido"
              | SOME t => typeMatch t tt
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
  | CallE ({func,args},_) => let val (formals, ret) =
                                   ( case tabFind vt func of
                                         NONE => raise Fail "func no def"
                                       | SOME (Func {formals,ret,...}) => (formals,ret)
                                       | SOME _ => raise Fail "no es func" )
                                 val seman_args = map seman' args
                                 val actual_types = map (#2) seman_args
                                 val _ = if length actual_types <> length formals
                                           then raise Fail "n_actuals != n_formals"
                                           else ()
                                 val pairs = ListPair.zip (actual_types,formals)
                             in if List.all (uncurry typeMatch) pairs
                                  then (SCAF, ret)
                                  else raise Fail "arg mismatch en call"
                             end
  | OpE ({left,oper,right}, _) =>
    let fun arith oo l r =
          let val (_,lt) = seman' l
              val (_,rt) = seman' r
          in if typeMatch lt TInt andalso typeMatch rt TInt
               then (SCAF, TInt)
               else raise Fail "oper aritmético sobre no-enteros"
          end
        fun eq oo l r =
          let val (_,lt) = seman' l
              val (_,rt) = seman' r
          in if typeMatch lt rt
                andalso not (lt = TNil andalso rt = TNil)
                andalso not (lt = TUnit)
               then (SCAF, TInt)
               else raise Fail "comparación inválida"
          end
        fun ord oo l r =
          let val (_,lt) = seman' l
              val (_,rt) = seman' r
          in if typeMatch lt rt andalso (typeMatch lt TInt orelse typeMatch lt TString)
               then (SCAF, TInt)
               else raise Fail "comparacion de orden inválida"
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
  | RecordE ({fields=flds, typ},_) =>
    let fun fldcmp ((n1,e1),(n2,e2)) = String.compare (n1,n2)
        val sorted_flds = Listsort.sort fldcmp flds
        val sorted_names = map (#1) sorted_flds
        val formal_types = case tabFind tt typ of
                             SOME (TRecord (fs,_)) => fs
                             | SOME _ => raise Fail "Not a record type"
                             | NONE => raise Fail "no existe el tipo"
    in if (map (#1) formal_types) <> sorted_names then
          raise Fail "faltan o sobran campos papá, te debo un mejor mensaje de error"
       else
          let val actual_types = map ((#2)o seman' o(#2)) sorted_flds
              fun matches (f::fs) (a::aa) = if typeMatch f a then matches fs aa else raise Fail "type mismatch en record"
                | matches [] [] = true
                | matches _ _ = raise Fail "imposible"
          in ( matches (map (#2) formal_types) actual_types ;
               (SCAF, tabTake tt typ) )
          end
    end
  | SeqE (es, _) =>
    let val semans = map seman' es
        val last = List.last semans
    in (SCAF, #2 last) end
  | AssignE ({l,r},_) =>
    let val (li,lt) = varSeman vt tt l
        val (ri,rt) = seman' r
    in if typeMatch lt rt andalso lt <> TIntRO
         then (SCAF, TUnit)
         else raise Fail "asignacion invalida :)"
    end
  | IfE ({test,th,el=SOME el}, _) =>
    let val (_, testtip) = seman' test
    in if typeMatch testtip TInt
         then let val (_, lt) = seman' th
                  val (_, rt) = seman' el
               in if typeMatch lt rt
                     then (SCAF, lt)
                     else raise Fail "error, branches con distintos tipos"
               end
         else
           raise Fail "test no entero"
    end
  | IfE ({test,th, el=NONE}, _) =>
    let val (_, testtip) = seman' test
    in if typeMatch testtip TInt
         then let val (_, lt) = seman' th
               in if typeMatch lt TUnit
                     then (SCAF, TUnit)
                     else raise Fail "error, if imperativo con tipo no unit"
               end
         else
           raise Fail "test no entero"
    end
  | WhileE ({test,body},_) =>
    let val (_, testt) = seman' test
        val _ = pushLoopLabel ()
        val (_, bodyt) = seman' body
        val _ = popLoopLabel ()
      in if typeMatch testt TInt andalso typeMatch bodyt TUnit
           then (SCAF, TUnit)
           else raise Fail "Error en while, test no entero o cuerpo no int."
      end
  | ForE ({index,lo,hi,body,...},_) =>
    let val (_,lot) = seman' lo
        val (_,hit) = seman' hi
        val _ = if not (typeMatch lot TInt) then raise Fail "error en for: low no int" else ()
        val _ = if not (typeMatch hit TInt) then raise Fail "error en for: high no int" else ()
        val newvt = tabCopy vt
        val _ = tabReplace newvt (index, Var TIntRO)
        val _ = pushLoopLabel ()
        val (bodyir, bodyt) = seman newvt tt body
        val _ = popLoopLabel ()
     in if typeMatch bodyt TUnit
          then (SCAF, TUnit)
          else raise Fail "for: cuerpo no unit"
     end
  | LetE ({decs, body},_) =>
    let fun proc_decl (dec,(vt', tt')) = declSeman vt' tt' dec
        val (newvt, newtt) = foldl proc_decl (vt,tt) decs
    in seman newvt newtt body
    end
  | BreakE _ => if !labelStack = [] then raise Fail "Break fuera de loop" else (SCAF, TUnit)
  | ArrayE ({typ,size,init},_) =>
    let val tt = case tabFind tt typ of
                   SOME t => tipoReal t
                   | NONE => raise Fail "tipo no existente (en decl de array)"
        val (_,initt) = seman' init
        in if typeMatch tt initt
               then (SCAF, TArray (tt, ref ()))
               else raise Fail "Valor de inicializacion para array difiere del tipo."
        end
end
and varSeman vt tt (SimpleVar s) = ( case tabFind vt s of
                                       SOME (Var t) => (SCAF, t)
                                       | NONE => raise Fail "Variable no definida. (no tendría que agarrarlo el escapado??)"
                                       | _ => raise Fail (s^" no es variable") )
  | varSeman vt tt (IndexVar (arr,idx)) =
        let val elemt = case varSeman vt tt arr of
                          (_, TArray (e,_)) => e
                          | _ => raise Fail "lalaalal"
            val (_,idxt) = seman vt tt idx
        in if typeMatch idxt TInt
             then (SCAF, elemt)
             else raise Fail "subscript not int"
       end
  | varSeman vt tt (FieldVar (record,fld)) =
      let val flds = case varSeman vt tt record of
                       (_, TRecord (flds,_)) => flds
                       | _ => raise Fail "lelelele"
          val fldt = case List.filter (fn (s,_) => s = fld) flds of
                       [] => raise Fail "no existe el campo"
                       | [(_,typ)] => typ
                       | _ => raise Fail "que pasó che?"
      in (SCAF, fldt) end
and declSeman vt tt (VarDecl ({name,escape,typ,init},_)) =
      let val (initir,initt) = seman vt tt init
          val formaltype = case typ of
                             SOME typename => ( case tabFind tt typename of
                                                  SOME t => tipoReal t
                                                  | NONE => raise Fail ("Tipo no existente: "^typename^".")
                                              )
                             | NONE => initt
      in if typeMatch formaltype initt
           then let val newvt = tabCopy vt
                    val _ = tabReplace newvt (name, Var formaltype)
                in (newvt, tt) end
           else raise Fail "tipo inferido y declarado difieren"
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
          fun proc_one (fd, info) =
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
              in if typeMatch bodyt rettype
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
          fun dep ({name,ty}, info) =
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
