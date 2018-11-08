open! Stdune

(* Based on the work by:

Michael A. Bender, Jeremy T. Fineman, Seth Gilbert, and Robert E. Tarjan. 2015. A New Approach to Incremental Cycle Detection and Related Problems. ACM Trans. Algorithms 12, 2, Article 14 (December 2015), 22 pages. DOI: https://doi.org/10.1145/2756553

*)

module Kind = struct
  type 'a tag = ..

  module type S = sig
    type t
    type 'a tag += X : t tag
  end

  type 'a t = (module S with type t = 'a)

  let create (type a) () =
    let module M = struct
      type t = a
      type 'a tag += X : t tag
    end in
    (module M : S with type t = a)

  let eq (type a) (type b)
        (module A : S with type t = a)
        (module B : S with type t = b)
    : (a, b) Type_eq.t option =
    match A.X with
    | B.X -> Some Type_eq.T
    | _   -> None
end

type 'a t = {
  kind : 'a Kind.t;
  mutable num : int;
  mutable index : int;
  mutable arcs : int;
}

type 'a node = {
  dag : 'a t;
  data : 'a;
  id : int;
  mutable index : int;
  mutable level : int;
  mutable deps : 'a node list;
  mutable rev_deps : 'a node list;
}

type 'a cycle_error = {
  nodes : 'a node list;
  kind : 'a Kind.t;
}

type packed_cycle_error = PackedList : _ cycle_error -> packed_cycle_error

exception Cycle of packed_cycle_error

let create () = {
  kind = Kind.create ();
  num = 0;
  index = 0;
  arcs = 0;
}

let gen_index (dag : 'a t) =
  dag.index <- dag.index - 1;
  dag.index

let node dag data =
  dag.num <- dag.num + 1;
  {
    dag = dag;
    data = data;
    id = dag.num;
    index = gen_index dag;
    level = 1;
    deps = [];
    rev_deps = [];
  }

let get v = v.data

let delta dag =
  let n = dag.num in
  let m = dag.arcs in
  let delta = min (float m ** (1.0 /. 2.0)) (float n ** (2.0 /. 3.0)) |> int_of_float in
  delta

let add v w =
  assert (v.dag = w.dag);

  let dag = v.dag in
  let delta = delta dag in

  dag.arcs <- dag.arcs + 1;

  (* Printf.printf "new add %s -> %s\n%!" v.input w.input; *)
  let marked_ids = ref Int.Set.empty in
  let arcs = ref 0 in
  let f = ref [] in
  let b = ref [] in

  let rec bvisit y acc =
    marked_ids := Int.Set.union !marked_ids (Int.Set.singleton y.id);
    if List.exists y.rev_deps ~f:(fun x -> btraverse x y (x :: acc)) |> not then begin
      b := List.append !b [y];
      false end
    else
      true
  and btraverse x _y acc =
    if x.id = w.id then
       Cycle (PackedList { nodes = acc; kind = dag.kind }) |> raise;
    arcs := !arcs + 1;
    if !arcs >= delta then begin
      w.level <- v.level + 1;
      w.rev_deps <- [];
      b := [];
      true
    end else begin
      if Int.Set.mem !marked_ids x.id |> not then
        bvisit x acc
      else
        false
    end in

  let rec reconstruct_b_path y acc x =
    if x.id = y.id then
      Some (acc)
    else
    List.find_map ~f:(reconstruct_b_path y (x :: acc)) x.rev_deps in

  let rec fvisit x acc =
    List.iter x.deps ~f:(fun y -> ftraverse x y (y :: acc));
    f := x :: !f
  and ftraverse x y acc =
    if y.id = v.id || List.exists ~f:(fun n -> n.id = y.id) !b then begin
      let path = reconstruct_b_path y [] v |> Option.value_exn in
      Cycle (PackedList { nodes = List.append path acc; kind = dag.kind }) |> raise
    end;
    if y.level < w.level then begin
      y.level <- w.level;
      y.rev_deps <- [];
      fvisit y acc
    end;
    if y.level = w.level then
      y.rev_deps <- x :: y.rev_deps in

  (* step 1: test order *)
  if (v.level < w.level || (v.level = w.level && v.index < w.index)) |> not then
  begin
    (* step 2 *)
    let step2res = bvisit v [v; w] in

    let acc = [w; v] in
    (* step 3 *)
    if step2res then
      fvisit w acc
    else if w.level <> v.level then begin
      w.level <- v.level;
      w.rev_deps <- [];
      fvisit w acc
    end;

    (* step 4 *)
    let l = List.rev (List.append !b !f) in
    List.iter ~f:(fun x -> x.index <- gen_index dag) l
  end;

  (* step 5 *)
  v.deps <- w :: v.deps;
  if v.level = w.level then
    w.rev_deps <- v :: w.rev_deps

let children node = node.deps

let pp_list pp_content fmt v =
  List.iter ~f:(fun f -> Format.fprintf fmt "%a;@, " pp_content f) v

let rec pp_depth depth pp_content fmt n =
  if depth >= 20 then
    Format.fprintf fmt "..."
  else
    Format.fprintf fmt "(%d: k=%d, i=%d) (%a) [@[%a@]]" n.id n.level n.index pp_content (get n) (pp_depth (depth + 1) pp_content |> pp_list) n.deps

let pp pp_content fmt n =
  pp_depth 0 pp_content fmt n

let is_child v w =
  v.deps |> List.exists ~f:(fun c -> c.id = w.id)

let unpack_list : type a. a t -> packed_cycle_error -> a node list = fun dag v ->
  match v with
  | PackedList v ->
    let Type_eq.T = Kind.eq (v.kind) (dag.kind) |> Option.value_exn in
    v.nodes

let dag node = node.dag
