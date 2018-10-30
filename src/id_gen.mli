
type idgen 
type t 

val peek : idgen -> t
val create : unit -> idgen
val to_int : t -> int
val gen : idgen -> t

module Set : Set.S with type elt = t

