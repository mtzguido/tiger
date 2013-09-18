open ast

val verbose = ref false

exception VarNoDec of symbol
exception ParseError
