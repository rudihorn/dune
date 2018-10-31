open! Stdune

type 'a t

type 'a node 

val create : unit -> 'a t

val node : 'a t -> 'a -> 'a node

val add : 'a node -> 'a node -> unit

val get : 'a node -> 'a

val children : 'a node -> 'a node list

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a node -> unit

val delta : 'a t -> int

val is_child : 'a node -> 'a node -> bool
