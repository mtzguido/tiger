open ast

val verbose = ref false

fun foldl1 ff ll = foldl ff (hd ll) (tl ll)

exception VarNoDec of symbol
exception ParseError

fun init l = List.take (l, length l - 1)

fun list_decor' [] = ""
  | list_decor' [x] = x
  | list_decor' (h::t) = h ^ ", " ^ list_decor' t

fun list_decor l = "[" ^ list_decor' l ^ "]"
