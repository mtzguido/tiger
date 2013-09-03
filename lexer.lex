{
open parser
open lineno

val comment_level = ref 0

fun inc x = (x := !x + 1; !x)
fun dec x = (x := !x - 1; !x)

fun escape1 s = escape1' (explode s)
and escape1' (#"^" :: x :: []) = let val n = ord x
                                     val c = chr (n-ord(#"@"))
                                 in implode (c::[]) end
|   escape1' _ = raise Fail "Scanner: Error interno 1"

fun escape2 str = escape2' (explode str)
and escape2' (a::b::c::[]) = let val oz = ord #"0"
                             in Char.toString (chr (64 * (ord a - oz) + 8 * (ord b - oz) + (ord c - oz)))
                             end
|   escape2' _ = raise Fail "Scanner: Error interno 2"

val atoi = valOf o Int.fromString

fun id_or_keyword "type" = TYPE
|	id_or_keyword "array" = ARRAY
|	id_or_keyword "of" = OF
|	id_or_keyword "var" = VAR
|	id_or_keyword "function" = FUNCTION
|	id_or_keyword "let" = LET
|	id_or_keyword "in" = IN
|	id_or_keyword "end" = END
|	id_or_keyword "if" = IF
|	id_or_keyword "then" = THEN
|	id_or_keyword "else" = ELSE
|	id_or_keyword "while" = WHILE
|	id_or_keyword "do" = DO
|	id_or_keyword "for" = FOR
|	id_or_keyword "to" = TO
|	id_or_keyword "break" = BREAK
|	id_or_keyword "nil" = NIL
|	id_or_keyword x = IDENT x

}

let SPC = [` ``\t``\r``\^L`]
let L = [`a`-`z``A`-`Z`]
let LDU = [`a`-`z``A`-`Z``0`-`9``_`]
let D = [`0`-`9`]

rule Tok =
	parse "/*" { inc comment_level; Com lexbuf }
	| eof { EOF }
	| "\n" { inc lineno; Tok lexbuf }
	| SPC+ { Tok lexbuf }
	| ":=" { DOSPIG }
	| ":" { DOSP }
	| "." { PUNTO }
	| ";" { PCOMA }
	| "," { COMA }
	| "=" { EQ }
	| "<" { LT }
	| ">" { GT }
	| ">=" { GEQ }
	| "<=" { LEQ }
	| "<>" { NEQ }
	| "(" { PI }
	| ")" { PD }
	| "{" { LI }
	| "}" { LD }
	| "[" { CI }
	| "]" { CD }
	| "&" { AMPER }
	| "|" { PIPE }
	| "+" { PLUS }
	| "-" { MINUS }
	| "/" { DIV }
	| "*" { MULT }
	| D+ { NRO (atoi (getLexeme lexbuf)) }
	| L LDU* { id_or_keyword (getLexeme lexbuf) }
	| "\"" { LITERAL (String lexbuf) }
    | _ { raise Fail ("Scanner: WUT ("^(getLexeme lexbuf)^").") }


and String =
	parse "\\" { StrEscape lexbuf }
	| "\"" { "" }
	| "\n" { raise Fail "Scanner: Newline en string literal." }
	| eof { raise Fail "Scanner: EOF en string literal." }
	| _ {	let val c = getLexeme lexbuf in
				if c > "\^_" then 	c^(String lexbuf)
				else 	raise Fail ("Scanner: Caracter inválido ("^c^") en cadena.")
			end
	}


and StrEscape = 
	parse eof { raise Fail "Scanner: EOF en string literal." }
	| "n" { "\n"^(String lexbuf) }
	| "t" { "\t"^(String lexbuf) }
	| "\\" { "\\"^(String lexbuf) }
	| "\"" { "\""^(String lexbuf) }
	| "^"[`@`-`_`] { (escape1 (getLexeme lexbuf))^(String lexbuf) }
	| D D D { (escape2 (getLexeme lexbuf))^(String lexbuf) } 
	| _ { raise Fail ("Scanner: Escape no reconocido ("^(getLexeme lexbuf)^").") }


and Com = 
	parse "/*" { inc comment_level; Com lexbuf }
	| "*/" { (if dec comment_level = 0 then Tok else Com) lexbuf }
	| "\n" { inc lineno ; Com lexbuf}
	| eof { raise Fail "Scanner: EOF mientras se procesaba comentario." }
	| _ { Com lexbuf }

;
