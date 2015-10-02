open ast

val verbose = ref false

fun foldl1 ff ll = foldl ff (hd ll) (tl ll)

exception VarNoDec of symbol
exception ParseError

fun init l = List.take (l, length l - 1)
