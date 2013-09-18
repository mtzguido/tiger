structure semantics :> semantics =
struct

open ast hash types

datatype IR = SCAF
datatype EnvEntry =
    VIntRO 
  | Var of TigerType
  | Func of {formals:TigerType list, ret:TigerType, extern:bool, label:string}

val init_venv : (symbol, EnvEntry) Tabla  = tabNew ()
val init_tenv : (symbol, TigerType) Tabla = tabNew ()

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
  | (TInt _, TInt _)   => true (* no importa acc? *)
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
  | IntE (i,_) => (SCAF, TInt RO) (* no deberia importar que sea RO, ya que no es l_value *)
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
                                  else raise Fail "arg mismatch"
                             end
  | OpE ({left,oper,right}, _) =>
    let fun arith oo l r =
          let val (_,lt) = seman vt tt l
              val (_,rt) = seman vt tt r
          in if typeMatch lt (TInt RW) andalso typeMatch rt (TInt RW)
               then (SCAF, TInt RW)
               else raise Fail "oper aritmético sobre no-enteros"
          end
        fun eq oo l r = 
          let val (_,lt) = seman vt tt l
              val (_,rt) = seman vt tt r
          in if typeMatch lt rt
                andalso not (lt = TNil andalso rt = TNil)
                andalso not (lt = TUnit)
               then (SCAF, TInt RW)
               else raise Fail "comparación inválida"
          end
        fun ord oo l r =  
          let val (_,lt) = seman vt tt l
              val (_,rt) = seman vt tt r
          in if typeMatch lt rt andalso (typeMatch lt (TInt RW) orelse typeMatch lt TString)
               then (SCAF, TInt RW)
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
  | RecordE _ => (SCAF, TUnit)
  | SeqE _ => (SCAF, TUnit)
  | AssignE _ => (SCAF, TUnit)
  | IfE _ => (SCAF, TUnit)
  | WhileE _ => (SCAF, TUnit)
  | ForE _ => (SCAF, TUnit)
  | LetE _ => (SCAF, TUnit)
  | BreakE _ => (SCAF, TUnit)
  | ArrayE _ => (SCAF, TUnit)

and varSeman vt tt (SimpleVar s) = ( case tabFind vt s of
                                       SOME (Var t) => (SCAF, t)
                                       | NONE => raise Fail "var no def"
                                       | _ => raise Fail "no es variable" )
  | varSeman vt tt (IndexVar (arr,idx)) =
        let val elemt = case varSeman vt tt arr of
                          (_, TArray (e,_)) => e
                          | _ => raise Fail "lalaalal"
            val (_,idxt) = seman vt tt idx
        in if typeMatch idxt (TInt RW)
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
and declSeman vt tt _ = (vt,tt)

fun semantics tree = (seman init_venv init_tenv tree ; print "tipado ok\n")

end (* struct *)
