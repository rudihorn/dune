open! Stdune

module Kind : sig
  type 'a t

  val eq : 'a t -> 'b t -> ('a, 'b) Type_eq.t option
end

module type S = sig
  type t

  val kind : t Kind.t
end

module Make
     (T : sig
        type t
      end)
     : S with type t = T.t

type 'a t

type 'a node 

type 'a cycle_error = {
  nodes : 'a node list;
  kind : 'a Kind.t;
}

type packed_cycle_error = PackedList : _ cycle_error -> packed_cycle_error

exception Cycle of packed_cycle_error

val create : unit -> 'a t

val node : 'a t -> 'a -> 'a node

val add : 'a node -> 'a node -> unit

val get : 'a node -> 'a

val children : 'a node -> 'a node list

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a node -> unit

val delta : 'a t -> int

val is_child : 'a node -> 'a node -> bool

val unpack_list : 'a t -> packed_cycle_error -> 'a node list

val dag : 'a node -> 'a t

