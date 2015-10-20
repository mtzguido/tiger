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

fun index x [] = raise Fail "not elem"
  | index x (h::t) = if x = h then 0 else 1 + index x t

fun index_safe x [] = NONE
  | index_safe x (h::t) = if x = h
                              then SOME 0
                              else (case index_safe x t of
                                       SOME v => SOME (v + 1)
                                     | NONE => NONE)

fun del x [] = []
  | del x (h::t) =
    if x = h
    then t
    else h :: (del x t)

fun ldiff l r =
    foldl (fn (e,a) => del e a) l r

fun printInt i =
    let val p = Int.toString i in
        if String.isPrefix "~" p
        then "-" ^ String.extract(p,1,NONE)
        else p
    end

fun indent s = "\t" ^ implode (indent' (explode s))
and indent' [] = []
  | indent' (h::t) = if h = #"\n"
                     then h :: #"\t" :: indent' t
                     else h :: indent' t
