type t = int
let to_int x = x

module IdGen = struct
  type idgen = int ref

  let create () = ref 0

  let to_int = to_int

  let gen counter =
    let n = !counter in
    counter := n + 1;
    n

  let peek counter =
    !counter + 1
end

module M = struct
  type t = int
  let compare (x : int) y = Int.compare x y
end

module Set = Set.Make(M)

module type IdMod = sig
  val idgen : IdGen.idgen
  val gen : unit -> t
  val peek : unit -> t
  val to_int : t -> int
end

module Make () : IdMod = struct
  let idgen = IdGen.create ()
  let gen () = IdGen.gen idgen
  let peek () = IdGen.peek idgen
  let to_int = IdGen.to_int
end


