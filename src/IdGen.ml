
type idgen = int ref
type t = int

let create () = ref 0

let to_int x = x

let gen counter =
  let n = !counter in
  counter := n + 1;
  n

module M = struct
  type t = int
  let compare (x : int) y = compare x y
end

module Set = Set.Make(M)

(*
module IdGen = struct
  include IdGen_Make

  let idgen = IdGen_Make.create ()
  let gen () = IdGen_Make.gen idgen 
end
*)
