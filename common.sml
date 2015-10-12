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

fun cartesian l [] = []
  | cartesian l (h::t) = (List.map (fn l => (l,h)) l) @ (cartesian l t)

fun uncurry f (x, y) = f x y
fun curry f x y = f (x, y)
