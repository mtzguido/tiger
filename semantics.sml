structure semantics :> semantics =
struct

open ast hash types

datatype IR = SCAF
datatype EnvEntry =
    Var of TigerType
  | Func of {formals:TigerType list, ret:TigerType, extern:bool, label:string}

val init_venv : (symbol, EnvEntry) Tabla  = tabNew ()
val init_tenv : (symbol, TigerType) Tabla = tabNew ()

val _ = tabInsertList init_tenv [("R", TRecord ([("a",TInt),("b",TInt),("c",TInt)], ref ()))]
val _ = tabInsertList init_venv [("x", Var TIntRO)]

fun uncurry f (x,y) = f x y

fun tipoReal t = case t of
    TSinom (_, r) => ( case !r of
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
  | (tt, TSinom (_,tref))
       => ( case !tref of
              NONE => raise Fail "sinónimo inválido"
              | SOME t => typeMatch tt t
          )
  | (TSinom (_,tref), tt)
       => ( case !tref of
              NONE => raise Fail "sinónimo inválido"
              | SOME t => typeMatch t tt
          )
  | (_,_) => false

fun seman vt tt exp = case exp of
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
                                 val seman_args = map (seman vt tt) args
                                 val actual_types = map (#2) seman_args
                                 val pairs = ListPair.zip (actual_types,formals)
                             in if List.all (uncurry typeMatch) pairs
                                  then (SCAF, ret)
                                  else raise Fail "arg mismatch en call"
                             end
  | OpE ({left,oper,right}, _) =>
    let fun arith oo l r =
          let val (_,lt) = seman vt tt l
              val (_,rt) = seman vt tt r
          in if typeMatch lt TInt andalso typeMatch rt TInt
               then (SCAF, TInt)
               else raise Fail "oper aritmético sobre no-enteros"
          end
        fun eq oo l r = 
          let val (_,lt) = seman vt tt l
              val (_,rt) = seman vt tt r
          in if typeMatch lt rt
                andalso not (lt = TNil andalso rt = TNil)
                andalso not (lt = TUnit)
               then (SCAF, TInt)
               else raise Fail "comparación inválida"
          end
        fun ord oo l r =  
          let val (_,lt) = seman vt tt l
              val (_,rt) = seman vt tt r
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
          let val actual_types = map ((#2)o(seman vt tt)o(#2)) sorted_flds
              fun matches (f::fs) (a::aa) = if typeMatch f a then matches fs aa else raise Fail "type mismatch en record"
                | matches [] [] = true
                | matches _ _ = raise Fail "imposible"
          in ( matches (map (#2) formal_types) actual_types ;
               (SCAF, tabTake tt typ) )
          end
    end
  | SeqE (es, _) =>
    let val semans = map (seman vt tt) es
        val last = List.last semans
    in (SCAF, #2 last) end
  | AssignE ({l,r},_) => 
    let val (li,lt) = varSeman vt tt l
        val (ri,rt) = seman vt tt r
    in if typeMatch lt rt andalso lt <> TIntRO
         then (SCAF, TUnit)
         else raise Fail "asignacion invalida :)"
    end
  | IfE ({test,th,el=SOME el}, _) =>
    let val (_, testtip) = seman vt tt test
    in if typeMatch testtip TInt
         then let val (_, lt) = seman vt tt th
                  val (_, rt) = seman vt tt el
               in if typeMatch lt rt
                     then (SCAF, lt)
                     else raise Fail "error, branches con distintos tipos"
               end
         else
           raise Fail "test no entero"
    end
  | IfE ({test,th, el=NONE}, _) =>
    let val (_, testtip) = seman vt tt test
    in if typeMatch testtip TInt
         then let val (_, lt) = seman vt tt th
               in if typeMatch lt TUnit
                     then (SCAF, TUnit)
                     else raise Fail "error, if imperativo con tipo no unit"
               end
         else
           raise Fail "test no entero"
    end
  | WhileE ({test,body},_) =>
    let val (_, bodyt) = seman vt tt body
        val (_, testt) = seman vt tt test
      in if typeMatch testt TInt andalso typeMatch bodyt TUnit
           then (SCAF, TUnit)
           else raise Fail "Error en while, test no entero o cuerpo no int."
      end
  | ForE ({index,lo,hi,body,...},_) =>
    let val (_,lot) = seman vt tt lo
        val (_,hit) = seman vt tt hi
        val _ = if not (typeMatch lot TInt) then raise Fail "error en for: low no int" else ()
        val _ = if not (typeMatch hit TInt) then raise Fail "error en for: high no int" else ()
        val newvt = tabCopy vt 
        val _ = tabReplace newvt (index, Var TIntRO)
        val (bodyir, bodyt) = seman newvt tt body
     in if typeMatch bodyt TUnit
          then (SCAF, TUnit)
          else raise Fail "for: cuerpo no unit"
     end
  | LetE ({decs, body},_) =>
    let fun proc_decl (dec,(vt', tt')) = declSeman vt' tt' dec
        val (newvt, newtt) = foldl proc_decl (vt,tt) decs
    in seman newvt newtt body
    end
  | BreakE _ => (SCAF, TUnit)
  | ArrayE ({typ,size,init},_) =>
    let val tt = case tabFind tt typ of
                   SOME t => tipoReal t
                   | NONE => raise Fail "tipo no existente (en decl de array)"
        in (SCAF, TArray (tt, ref ())) end
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
and declSeman vt tt (FuncDecl _) = (vt,tt)
  | declSeman vt tt (TypeDecl _) = (vt,tt)
  | declSeman vt tt (VarDecl _) = (vt,tt)

fun semantics tree = (seman init_venv init_tenv tree ; print "tipado ok\n")

end (* struct *)
