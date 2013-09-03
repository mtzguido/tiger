open lexer
open parser
open ast
open BasicIO Nonstdio Lexing

fun lexstream(is: instream) =
    Lexing.createLexer(fn b => fn n => buff_input is b 0 n);


fun main(args) =
	let	val entrada = if args = [] then std_in
                      else open_in (hd args)
        val res = prog Tok (lexstream entrada)
    in print "\t Parseado ok\n"
    end handle _ => print "\t Error en parsing\n"

val _ = main(CommandLine.arguments())
