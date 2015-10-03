open BasicIO Nonstdio Lexing List Process

open lexer parser ast escape common semantics

fun lexstream(is: instream) =
    Lexing.createLexer(fn b => fn n => buff_input is b 0 n);

exception NoInput
exception ParseErr of int * string

fun err (VarNoDec name)  = print ("Error: variable no definida ("^name^").\n")
  | err (Fail s)         = print ("Error: "^s^".\n")
  | err (SysErr (s,_))   = print ("Error del sistema: "^s^".\n")
  | err (ParseErr (l,t)) = ( print ("Error de parsing en línea "^(makestring l)^"\n");
                             print ("Token inesperado: \""^t^"\"\n")
                           )
  | err  SemanFail       = print "Error de semántica.\n"
  | err  NoInput         = print (CommandLine.name()^": no input files\n")
  | err  Subscript       = print "Error interno: Subscript\n"
  | err x = print "Excepción no reconocida!\n"

fun printTokens lbuf =
    let fun onetok _ = let val tt = Tok lbuf
                       in if tt <> EOF then
                             ( (print o tokStr)(tt); print "  "; onetok () )
                          else ()
                       end
    in onetok () end
and tokStr tok = case tok of TYPE => "type" | ARRAY => "array" | OF => "of" | VAR => "var" | FUNCTION => "function" | LET => "let" | IN => "in" | END => "end" | IF => "if" | THEN => "then" | ELSE => "else" | WHILE => "while" | DO => "do" | FOR => "for" | TO => "to" | BREAK => "break" | NIL => "nil" | IDENT x => "IDENT ("^x^")" | DOSPIG => ":=" | DOSP => ":" | PUNTO => "." | PCOMA => ";" | COMA => "," | EQ => "=" | LT => "<" | GT => ">" | GEQ => ">=" | LEQ => "<=" | NEQ => "<>" | PI => "(" | PD => ")" | LI => "{" | LD => "}" | CI => "[" | CD => "]" | AMPER => "&" | PIPE => "|" | PLUS => "+" | MINUS => "-" | DIV => "/" | MULT => "*" | NRO n => "NUM ("^(makestring n)^")" | LITERAL s => "LITERAL ("^s^")" | _ => raise Fail "Token no reconocido"

val _ =
let
    val (opts,files) = partition
                           (fn s => hd (explode s) = #"-" andalso s <> "-")
                           (CommandLine.arguments())

    val entrada      = if length files > 1 then
                           raise Fail "solo puedo compilar de a un archivo!"
                       else if length files = 0 then
                           raise NoInput
                       else if hd files = "-" then
                           std_in
                       else
                            open_in (hd files)
    fun haveOpt s    = exists (fn e => e = s) opts
    val verboseOpt   = haveOpt "-v"
    val tokOpt       = haveOpt "-tokens"
    val noEscape     = haveOpt "-noescape"
    val _            = verbose := verboseOpt (* seteamos verbose *)
    val lexbuf       = lexstream entrada
    val _            = if tokOpt then (printTokens lexbuf; exit success) else ()
    val ast          = prog Tok lexbuf handle _ => raise ParseErr (!lineno, Lexing.getLexeme lexbuf)
in if !verbose then print "Parsing finalizado OK.\n" else ();
   if not noEscape then marcarEscapes ast else () ;
   if !verbose then print "Escapes marcados\n" else () ;
   semantics ast ;
   print "COMPILATION OK\n"
end handle x => (err x ; print "COMPILATION FAILED\n"; Process.exit failure)
