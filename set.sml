structure set :> set =
struct
    type 'a set = 'a Binaryset.set

    fun emptySet f = Binaryset.empty f

    fun insert e s = Binaryset.add (s, e)
    fun delete e s = Binaryset.delete (s, e) handle NotFound => s
    fun member e s = Binaryset.member (s, e)
    fun tolist s = Binaryset.listItems s

    fun singleton f e = Binaryset.singleton f e

    fun fromlist f l = Binaryset.addList (emptySet f, l)

    fun union s r = Binaryset.union (s, r)
    fun diff s r  = Binaryset.difference (s, r)

    fun size s = Binaryset.numItems s

    fun find p s = Binaryset.find p s

    fun foldl f e s = Binaryset.foldl f e s
end
