structure hash :> hash =
struct
    open Polyhash (* de aca viene la magia *)

    exception Miss
    exception Dup
    type (''a, 'b) Tabla = (''a, 'b) hash_table

    fun tabNew () = mkPolyTable (100, Miss)
    fun tabCopy tt = copy tt

    fun tabInsert t (k,v) = case peekInsert t (k,v) of
                              SOME _ => raise Dup
                              | NONE => ()
    fun tabReplace t (k,v) = insert t (k,v)

    fun tabHas t k = case peek t k of
                       NONE => false
                       | _ => true
    fun tabFind t k = peek t k
    fun tabTake t k = find t k

    fun tabDel t k =
        (remove t k; ()) handle _ => ()

    fun tabToList t = listItems t
    fun tabInsertList t l = List.app (tabInsert t) l
    fun tabReplaceList t l = List.app (tabReplace t) l

    fun tabMap t f =
        map f t
end
