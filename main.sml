open BasicIO Nonstdio Lexing List Process

open lexer parser ast escape common

fun lexstream(is: instream) =
    Lexing.createLexer(fn b => fn n => buff_input is b 0 n);

fun printAst ast = print "Aca va el AST...\n"

fun err (VarNoDec name) = print ("Error: variable no definida ("^name^").\n")
  | err (Fail s)        = print ("Error: "^s^".\n")
  | err (SysErr (s,_))  = print ("Error del sistema: "^s^".\n")
  | err  ParseError     = print "Error de parsing.\n"
  | err x = raise x

fun printTokens lbuf = 
    let fun onetok _ = let val tt = Tok lbuf
                       in if tt <> EOF then                                                   
                             ( (print o tokStr)(tt); print "  "; onetok () )                   
                          else () 
                       end
    in onetok () end
and tokStr tok = case tok of TYPE => "type" | ARRAY => "array" | OF => "of" | VAR => "var" | FUNCTION => "function" | LET => "let" | IN => "in" | END => "end" | IF => "if" | THEN => "then" | ELSE => "else" | WHILE => "while" | DO => "do" | FOR => "for" | TO => "to" | BREAK => "break" | NIL => "nil" | IDENT x => "IDENT ("^x^")" | DOSPIG => ":=" | DOSP => ":" | PUNTO => "." | PCOMA => ";" | COMA => "," | EQ => "=" | LT => "<" | GT => ">" | GEQ => ">=" | LEQ => "<=" | NEQ => "<>" | PI => "(" | PD => ")" | LI => "{" | LD => "}" | CI => "[" | CD => "]" | AMPER => "&" | PIPE => "|" | PLUS => "+" | MINUS => "-" | DIV => "/" | MULT => "*" | NRO n => "NUM ("^(makestring n)^")" | LITERAL s => "LITERAL ("^s^")" | _ => raise Fail "Token no reconocido"

val (opts,files) = partition (fn s => hd (explode s) = #"-") (CommandLine.arguments())
val _ = if files = [] then raise Fail "no se dieron archivos de entrada" else ()
val _ = if tl files = [] then () else raise Fail "solo puedo compilar de a un archivo!"
val entrada      = if files = [] then std_in else open_in (hd files)
fun haveOpt s    = exists (fn e => e = s) opts
val print_ast    = haveOpt "-ast"
val verboseOpt   = haveOpt "-v"
val tokOpt       = haveOpt "-tokens"
val _            = verbose := verboseOpt (* seteamos verbose *)
val _            = if tokOpt then (printTokens (lexstream entrada); exit success) else ()
val ast          = prog Tok (lexstream entrada) handle _ => raise ParseError

val run = ( if !verbose then print "Parsing finalizado OK.\n" else ();
            if print_ast then printAst ast else () ;
            marcarEscapes ast ;
            if !verbose then print "Escapes marcados\n" else ()
          ) handle x => err x
