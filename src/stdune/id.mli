module type S = sig
  type t

  module Set : Set_intf.S with type elt = t

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
module Make () : S
