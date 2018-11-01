open !Set

type t

val to_int : t -> int

module IdGen : sig
  type idgen 

  val peek : idgen -> t
  val create : unit -> idgen
  val to_int : t -> int
  val gen : idgen -> t
end

module Set : Set_intf.S with type elt = t

module type IdMod = sig
  val idgen : IdGen.idgen
  val gen : unit -> t
  val peek : unit -> t
  val to_int : t -> int
end

module Make () : IdMod
