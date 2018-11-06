open !Set

type t

(** Convert an ID to an integer. *)
val to_int : t -> int

(** The internal IdGen module which generates an
    id using an instance of an ID generator for
    the next ID. *)
module IdGen : sig
  type idgen 

  val peek : idgen -> t
  val create : unit -> idgen
  val to_int : t -> int
  val gen : idgen -> t
end

module Set : Set_intf.S with type elt = t

module type IdMod = sig
  type t

  module Set : Set_intf.S with type elt = t

  val idgen : IdGen.idgen

  (** Generate a new id. *)
  val gen : unit -> t

  (** Get the next id that would be generated, without actually
      generating it. *)
  val peek : unit -> t

  (** Convert the id to an integer. *)
  val to_int : t -> int

  (** Compare two ids. *)
  val compare : t -> t -> Ordering.t
end

(** A functor to create a new ID generator module. *)
module Make () : IdMod
