signature hash =
sig

(* las operaciones modifican la tabla pasada como parametro.
   si se desea que no haya efectos lateral, usar tabCopy *)

exception Dup
exception Miss

type (''a, 'b) Tabla
val tabNew    :  unit -> (''a, 'b) Tabla
val tabCopy   : (''a, 'b) Tabla -> (''a, 'b) Tabla

val tabInsert  : (''a, 'b) Tabla -> (''a * 'b) -> unit (* lanza excepcion si ya está *)
val tabReplace : (''a, 'b) Tabla -> (''a * 'b) -> unit (* agrega, posiblemente sobreescribiendo *)

val tabHas  : (''a, 'b) Tabla -> ''a -> bool
val tabFind : (''a, 'b) Tabla -> ''a -> 'b option
val tabTake : (''a, 'b) Tabla -> ''a -> 'b             (* lanza excepcion si no está *)

val tabDel : (''a, 'b) Tabla -> ''a -> unit

val tabToList : (''a, 'b) Tabla -> (''a * 'b) list
val tabInsertList  : (''a, 'b) Tabla -> (''a * 'b) list -> unit
val tabReplaceList : (''a, 'b) Tabla -> (''a * 'b) list -> unit

(* copia la tabla *)
val tabMap : (''a, 'b) Tabla -> (''a * 'b -> 'b) -> (''a, 'b) Tabla

end
