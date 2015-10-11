structure set :> set =
struct
    type 'a set = 'a list * ('a -> 'a -> bool)

    fun beq a b = a = b

    val emptySet = ([], beq)
    fun emptySet' f = ([], f)

    fun elem f x [] = false
      | elem f x (h::t) = f x h orelse elem f x t

    fun del f x [] = []
      | del f x (h::t) =
        if f x h
        then t
        else h :: (del f x t)

    fun insert x (l, f) =
        if elem f x l   
        then (l, f)
        else (x :: l, f)

    fun delete x (l, f) = (del f x l, f)

    fun member x (l, f) = elem f x l

    fun tolist (l, _) = l

    fun singleton e = insert e emptySet
    fun fromlist l = foldl (fn (e, s) => insert e s) emptySet l

    fun union (l, _) r =
        foldl (fn (e,s) => insert e s) r l

    fun diff l (r, _) =
        foldl (fn (e,s) => delete e s) l r

    fun size (l, _) = length l
end
