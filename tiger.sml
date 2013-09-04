open lexer parser ast escape

open BasicIO Nonstdio Lexing List

fun lexstream(is: instream) =
    Lexing.createLexer(fn b => fn n => buff_input is b 0 n);

fun printAst ast = print "Te lo debo!\n"

fun marcar_escapes _ = ()

fun main(args) =
	let	val (opts,files) = partition (fn s => hd (explode s) = #"-") args
        val entrada      = if files = [] then std_in else open_in (hd files)
        fun haveOpt s    = exists (fn e => e = s) opts
        val print_ast    = haveOpt "-ast"
        val print_escape = haveOpt "-escape"
        val ast          = prog Tok (lexstream entrada)
    in ( print "Parsing finalizado OK.\n" ;
         if print_ast then (print "AST: ";  printAst ast) else () ;
         marcarEscapes ast ;
         print "Escapes marcados\n"
       )
    end handle _ => print "\t Error en parsing\n"

val _ = main(CommandLine.arguments())
