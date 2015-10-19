%{

open lineno
open ast

fun getinfo () =
    makeinfo (!lineno)

fun typeDecMerge(FuncDecl [fd], (FuncDecl list)::rest) =
    (FuncDecl (fd::list))::rest
  | typeDecMerge(TypeDecl [td], (TypeDecl list)::rest) =
    (TypeDecl (td::list))::rest
  | typeDecMerge(l, r) =
    l::r

fun nombre (SimpleVar (s,_)) = s
  | nombre _ = raise Fail "Parser: Error interno 1"


%}

%token EOF
%token TYPE ARRAY OF VAR FUNCTION
%token LET IN END IF THEN ELSE WHILE DO FOR TO BREAK
%token PUNTO DOSP DOSPIG COMA PCOMA EQ PI PD CI CD LI LD
%token AMPER PIPE LT GT LEQ GEQ NEQ
%token PLUS MINUS MULT DIV NIL DEBUG
%token<int> NRO
%token<string> LITERAL IDENT

%type<ast.exp> exp prog
%type<ast.exp list> args
%type<ast.symbol> id
%type<ast.symbol list> ids
%type<(ast.symbol * ast.exp) list> flds
%type<ast.exp list> seq
%type<ast.decl list> decs
%type<ast.field list> tyflds
%type<ast.argument list> argsdec argsdec1
%type<ast.decl> dec tydec vardec fundec
%type<ast.var> lvalue
%type<ast.ty> ty

%nonassoc THEN
%left ELSE
%nonassoc OF
%nonassoc FOR TO DO
%nonassoc DOSPIG
%left PIPE
%left AMPER
%nonassoc GEQ LEQ NEQ EQ LT GT
%left PLUS MINUS
%left DIV MULT
%left DEBUG

%start prog

%%
prog : exp EOF { $1 }

exp : NRO { IntE ($1, getinfo()) }
    | PI PD { UnitE (getinfo()) }
    | LITERAL { StringE ($1, getinfo()) }
    | NIL { NilE (getinfo()) }
    | BREAK { BreakE (getinfo()) }
    | IF exp THEN exp ELSE exp { IfE ({test=$2, th=$4, el=SOME $6}, getinfo()) } /* if con else */
    | IF exp THEN exp          { IfE ({test=$2, th=$4, el=NONE   }, getinfo()) } /* if sin else */
    | id PI args PD { CallE ({func=$1, args=$3}, getinfo()) } /* llamada a función */
    | DEBUG exp { DebugE $2 } /* Debug! */

    /* operadores aritmeticos y de comparacion */
    | exp PLUS  exp { OpE ({left=$1, oper=PlusOp,  right=$3}, getinfo()) }
    | exp MINUS exp { OpE ({left=$1, oper=MinusOp, right=$3}, getinfo()) }
    | exp MULT  exp { OpE ({left=$1, oper=MultOp,  right=$3}, getinfo()) }
    | exp DIV   exp { OpE ({left=$1, oper=DivOp,   right=$3}, getinfo()) }
    | exp GEQ   exp { OpE ({left=$1, oper=GeOp,    right=$3}, getinfo()) }
    | exp LEQ   exp { OpE ({left=$1, oper=LeOp,    right=$3}, getinfo()) }
    | exp GT    exp { OpE ({left=$1, oper=GtOp,    right=$3}, getinfo()) }
    | exp LT    exp { OpE ({left=$1, oper=LtOp,    right=$3}, getinfo()) }
    | exp EQ    exp { OpE ({left=$1, oper=EqOp,    right=$3}, getinfo()) }
    | exp NEQ   exp { OpE ({left=$1, oper=NeqOp,   right=$3}, getinfo()) }

    | exp PIPE exp { IfE ({test=$1, th=(IntE (1,fakeinfo)), el=SOME $3}, getinfo()) } /* O lógico */
    | exp AMPER exp { IfE ({test=$1, th=$3, el=SOME (IntE (0,fakeinfo))}, getinfo()) } /* Y lógico */

    | id LI flds LD { RecordE ({ fields=$3, typ=$1 }, getinfo() ) } /* record creation */

    /* array creation */
    /* si aca ponemos id en vez de lvalue hay un conflicto shift-reduce, por qué? */
    | lvalue CI exp CD OF exp { ArrayE ({typ=nombre $1, size=$3, init=$6}, getinfo()) }
    | PI exp PCOMA seq PD { SeqE ($2::$4, getinfo()) } /* sequencing no vacio */
    | PI exp PD { $2 }  /* cambio de precedencia */

    | LET decs IN END { LetE ({decs=$2, body=UnitE fakeinfo}, getinfo()) } /* let */
    | LET decs IN exp END { LetE ({decs=$2, body=$4}, getinfo()) } /* let */
    | LET decs IN exp PCOMA seq END { LetE ({decs=$2, body= SeqE ($4::$6, fakeinfo)}, getinfo()) } /* let */

    | lvalue { VarE ($1, getinfo()) } /* un lvalue tambien es expresion */
    | lvalue DOSPIG exp { AssignE ({ l=$1, r=$3}, getinfo()) } /* asignación */
    | MINUS exp { OpE ({left=IntE(0,fakeinfo), oper=MinusOp, right=$2}, getinfo()) } /* negacion unaria */
    | WHILE exp DO exp { WhileE ({test=$2, body=$4}, getinfo()) } /* while */
    | FOR id DOSPIG exp TO exp DO exp { ForE ({index=$2, lo=$4, hi=$6, escape=ref false, body=$8}, getinfo()) } /* for */
    ;

decs: dec decs { typeDecMerge($1,$2) } /* declaraciones */
    | { [] }

dec : tydec { $1 }
    | vardec { $1 }
    | fundec { $1 }

tydec: TYPE id EQ ty { TypeDecl [({name=$2, ty=$4}, getinfo())] } /* declaracion de tipo */
     ;

ty  : id { NameTy $1 } /* sinónimo */
    | LI tyflds LD { RecordTy $2 } /* def record */
    | ARRAY OF id { ArrayTy $3 } /* def array */
    ;

tyflds : { [] }
       | id DOSP id { {name=$1, typ=$3} :: [] }
       | id DOSP id COMA tyflds {{ name=$1, typ=$3} :: $5 }
       ;

vardec : VAR id DOSPIG exp { VarDecl ({ name=$2, escape=ref false, typ=NONE, init=$4}, getinfo()) } /* def variable */
       | VAR id DOSP id DOSPIG exp { VarDecl ({ name=$2, escape=ref false, typ=SOME $4, init=$6}, getinfo()) } /* def variable con tipo */
       ;

fundec : FUNCTION id PI argsdec PD EQ exp { FuncDecl [({name=$2, params=$4, result=NONE, body=$7}, getinfo())] }
       | FUNCTION id PI argsdec PD DOSP id EQ exp { FuncDecl [({name=$2, params=$4, result=SOME $7, body=$9}, getinfo())] }
       ;

ids : id     { [$1] }
    | id ids { $1::$2 }

argsdec1 : ids DOSP id           { map (fn n => {name=n, typ=$3, escape=ref false}) $1 }

argsdec :                        { [] }
        |  argsdec1              { $1 }
        |  argsdec1 COMA argsdec { $1@$3 }
        ;

lvalue : id { SimpleVar ($1, getinfo ()) }
       | lvalue PUNTO id { FieldVar ($1, $3, getinfo ()) }
       | lvalue CI exp CD { IndexVar ($1, $3, getinfo ()) }
       ;

seq : exp PCOMA seq { $1 :: $3 }
    | exp { $1::[] }
    ;

flds :           { [] }
     | id EQ exp { [($1,$3)] }
     | id EQ exp COMA flds { ($1,$3)::$5 }
     ;

args : exp COMA args { $1::$3 }
     | exp { [$1] }
     | { [] }
     ;

id : IDENT { $1 }
   ;


%%
