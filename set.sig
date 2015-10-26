signature set =
sig
    type 'a set

    val emptySet : (''a * ''a -> order) -> ''a set
    val singleton : (''a * ''a -> order) -> ''a -> ''a set
    val fromlist : (''a * ''a -> order) -> ''a list -> ''a set

    val insert : 'a -> 'a set -> 'a set
    val delete : 'a -> 'a set -> 'a set
    val member : 'a -> 'a set -> bool
    val tolist : 'a set -> 'a list

    val union : 'a set -> 'a set -> 'a set
    val diff  : 'a set -> 'a set -> 'a set

    val size : 'a set -> int

    val find : ('a -> bool) -> 'a set -> 'a option
end
