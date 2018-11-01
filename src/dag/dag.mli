open! Stdune

(* Based on the work by:

Michael A. Bender, Jeremy T. Fineman, Seth Gilbert, and Robert E. Tarjan. 2015. A New Approach to Incremental Cycle Detection and Related Problems. ACM Trans. Algorithms 12, 2, Article 14 (December 2015), 22 pages. DOI: https://doi.org/10.1145/2756553

*)

module Kind : sig
  type 'a t

  val eq : 'a t -> 'b t -> ('a, 'b) Type_eq.t option
end

(** A DAG (Directed Acyclic Graph). *)
type 'a t

(** A node in the DAG. *)
type 'a node 

(** Unpacked cycle error data. *)
type 'a cycle_error = {
  nodes : 'a node list;
  kind : 'a Kind.t;
}

(** A packed list of nodes (to prevent needing a type variable). Use [unpack_list] to get the
    underlying nodes. *)
type packed_cycle_error = PackedList : _ cycle_error -> packed_cycle_error

(** An cycle has been found while adding an arc. *)
exception Cycle of packed_cycle_error

(** [create ()] creates a directed acyclic graph. *)
val create : unit -> 'a t

(** [node dag v] creates a new node that belongs to [dag] with the value [v].  *)
val node : 'a t -> 'a -> 'a node

(** [add v w] creates an arc going from [v] to [w].
    @raise Cycle if creating the arc would create a cycle. *)
val add : 'a node -> 'a node -> unit

(** [get v] gets the underlying value of the node [v]. *)
val get : 'a node -> 'a

(** [children v] returns all nodes [w] for which an arc going from [v] to [w] exists. *)
val children : 'a node -> 'a node list

(** When using a pretty printing function [format], [format "%a" (pp pp_node) n] formats
    the node using the value formatter [pp_node] for the underlying value. *)
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a node -> unit

(** This returns the \Delta value of the given dag. This is a maximum bound on the number
    of nodes to search and is useful for debugging. *)
val delta : 'a t -> int

(** [is_child v w] returns a boolean indicating if an arc going from [v] to [w] exists. *)
val is_child : 'a node -> 'a node -> bool

(** When [add v w] is called and it would create a cycle, the [Cycle] exception is thrown.
    The [Cycle] exception contains a packed list of nodes, which need to be unpacked to get the correct
    types. This can be done by calling [unpack_list dag packed]. *)
val unpack_list : 'a t -> packed_cycle_error -> 'a node list

(** [dag v] returns the underlying DAG for a given node [v]. *)
val dag : 'a node -> 'a t

