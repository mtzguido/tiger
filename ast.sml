structure ast :> ast =
struct

type symbol = string
type info = { pos: int }

val fakeinfo = { pos= 0 }

fun info2str ({pos}) = "(linea: "^(makestring pos)^")"
fun infoline ({pos}) = pos
fun makeinfo nl = {pos=nl}

datatype exp = VarE of var * info
             | UnitE of info
             | NilE of info
             | IntE of int * info
             | StringE of string * info
             | CallE of { func: symbol, args: exp list } * info
             | OpE of { left: exp, oper: oper, right:exp } * info
             | RecordE of { fields: (symbol*exp) list, typ: symbol } * info
             | SeqE of exp list * info
             | AssignE of { l:var, r:exp } * info
             | IfE of { test:exp, th:exp, el:exp option} * info
             | WhileE of { test:exp, body:exp } * info
             | ForE of { index:symbol, lo:exp, hi:exp,
                         escape: bool ref, body:exp } * info
             | LetE of { decs: decl list, body:exp } * info
             | BreakE of info
             | ArrayE of { typ: symbol, size:exp, init:exp } * info
             | DebugE of exp

and var = SimpleVar of symbol * info
        | FieldVar of var * symbol * info
        | IndexVar of var * exp * info

and decl = FuncDecl of ({ name:symbol, params: argument list,
                          result: symbol option, body:exp } * info) list
         | VarDecl of { name:symbol, escape:bool ref,
                        typ:symbol option, init: exp } * info
         | TypeDecl of ({name:symbol, ty:ty} * info) list

and ty = NameTy of symbol
       | RecordTy of field list
       | ArrayTy of symbol

and oper = PlusOp | MinusOp | MultOp | DivOp
         | EqOp | NeqOp | LtOp | GtOp | GeOp
         | LeOp

withtype field = {name:symbol, typ:symbol}
and   argument = {name:symbol, typ:symbol, escape:bool ref}
end
