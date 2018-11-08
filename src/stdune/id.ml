module type S = sig
  type t

  module Set : Set_intf.S with type elt = t

  val gen : unit -> t
  val peek : unit -> t
  val to_int : t -> int
  val compare : t -> t -> Ordering.t
end


module Make () = struct
  module Set = Int.Set

  type t = int

  let next = ref 0

  let gen () =
    let v = !next in
    next := v + 1;
    v

  let peek () =
    !next

  let to_int x = x

  let compare = Int.compare
end
