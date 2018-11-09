open !Stdune
open Import
(* open Staged *)
open Fiber.O

(* type used for names of memoized functions *)
type name = string

(* seralized input / output types *)
type ser_input = string
type ser_output = string

type 'a input_spec = {
  serialize : 'a -> ser_input;
  print : 'a -> string;
  not_equal : 'a -> 'a -> bool;
}
type cutoff_policy =
  | No_cutoff
  | Cutoff

type 'a output_spec = {
  serialize : 'a -> ser_output;
  print : 'a -> string;
  cutoff_policy : cutoff_policy;
}

(* a function that will try to recompute the memoized function to ensure
   the output is up-to-date *)
type update_cache = unit -> ser_output Fiber.t

module RunId = Id.Make ()
module FnId = Id.Make ()
module Id = Id.Make()

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

module Output = struct
  type 'output run_state =
    | Running of 'output Fiber.Ivar.t
    | Done of 'output

  type 'a output_cache = {
    state : 'a run_state;
    kind : 'a Kind.t;
  }

  type packed_output = Packed : _ output_cache -> packed_output

  let unpack : type a. a Kind.t -> packed_output -> a run_state =
    fun kind packed ->
      match packed with
      |  Packed output ->
        let Type_eq.T = Kind.eq (output.kind) kind |> Option.value_exn in
        output.state

  let pack kind v = Packed {
    state = v;
    kind = kind;
  }
end

type global_cache_info = {
  last_output : ser_output;
  mutable last_run : RunId.t;
  update : update_cache;
  cutoff_policy : cutoff_policy;
  last_deps : (dep_node * ser_output) list;
}

and dep_info = {
  name : name;
  input : ser_input;
  id : Id.t;
  mutable cache : global_cache_info option;
  mutable output : Output.packed_output option;
}

and dep_node = dep_info Dag.node

module Stack_frame = struct
  type t = {
    dep_node : dep_node;
  }

  let di v = Dag.get v.dep_node

  let name v = (di v).name
  let input v = (di v).input
end

module Cycle_error = struct
  type t = {
    cycle : dep_node list;
    stack : Stack_frame.t list;
  }

  exception E of t

  let stack ex = ex.stack

  let filter ~(name:name) ex =
    ex.cycle
    |> List.filter ~f:(fun (f : dep_node) -> (Dag.get f).name = name)
    |> List.map ~f:(fun (f : dep_node) -> (Dag.get f).input)

  let serialize ex =
    ex.cycle
    |> List.map ~f:Dag.get
    |> List.map ~f:(fun (f : dep_info) -> Format.sprintf "%s %s" f.name f.input)

  let serialize_stack ex =
    ex.stack
    |> List.map ~f:(fun v -> Stack_frame.di v)
    |> List.map ~f:(fun (f : dep_info) -> Format.sprintf "%s %s" f.name f.input)
end


let global_dep_dag = Dag.create ()
let global_dep_table : (FnId.t * ser_input, dep_node) Hashtbl.t = Hashtbl.create 256

let global_id_table : (name, FnId.t) Hashtbl.t = Hashtbl.create 256

module Fdecl = struct
  type ('a, 'b) t = 'a -> 'b Fiber.t

  type ('a, 'b) state =
  | Empty
  | Full of ('a, 'b) t

  type ('a, 'b) comp = {
    mutable state : ('a, 'b) state
  }

  let create () = { state = Empty }

  let set comp f =
    match comp.state with
    | Empty -> comp.state <- Full f
    | Full _ -> die "Computation already set."

  let get comp =
    match comp.state with
    | Empty -> die "Computation not set."
    | Full f -> f
end

module Memoize = struct
  (* fiber context variable keys *)
  let call_stack_key = Fiber.Var.create ()
  let get_call_stack_int =
    Fiber.Var.get call_stack_key (* get call stack *)
      >>| Option.value ~default:([], Id.Set.empty) (* default call stack is empty *)

  let get_call_stack =
    get_call_stack_int >>| (fun (stack,_) -> stack)

  let run_id = Fiber.Var.create ()
  let run_memoize fiber =
    Fiber.Var.set run_id (RunId.gen ()) fiber

  let get_run_id =
    Fiber.Var.get_exn run_id

  (* set up the fiber so that it both has an up to date call stack
     as well as an empty dependency table *)
  let wrap_fiber (stack_frame : Stack_frame.t) id (f : 'a Fiber.t) =
    (* set the context so that f has the call stack  *)
    get_call_stack_int
      >>| (fun (stack, set) -> stack_frame :: stack, Id.Set.add set id) (* add top entry *)
      >>= (fun stack -> Fiber.Var.set call_stack_key stack f) (* update *)

  let dump_stack v =
    get_call_stack
      >>|
        (Printf.printf "Memoized function stack:\n";
        List.iter ~f:(fun st -> Printf.printf "   %s %s\n" (Stack_frame.name st) (Stack_frame.input st)))
      >>| (fun _ -> v)

  let get_dependency_node (fn_id : FnId.t) (name : name) (inp : ser_input) : dep_node =
    Hashtbl.find global_dep_table (fn_id, inp)
    |> function
      | None ->
        let newId = Id.gen () in
        let entry = {
          id = newId;
          name = name;
          input = inp;
          cache = None;
          output = None;
        } in
        let node = Dag.node global_dep_dag entry in
        Hashtbl.replace global_dep_table ~key:(fn_id,inp) ~data:node;
        node
      | Some t -> t

  let add_rev_dep dep_node =
    get_call_stack
    >>| function
        | [] -> ()
        | x :: _ as stack ->
            let rev_dep = x.dep_node in
            (* if the caller doesn't already contain this as a dependent *)
            try
              if Dag.is_child rev_dep dep_node |> not then
                Dag.add rev_dep dep_node
            with Dag.Cycle packed ->
              let cycle = Dag.unpack_list (Dag.dag dep_node) packed in
              Cycle_error.E {
                stack = stack;
                cycle = cycle;
              } |> raise

  let get_name_id (name : name) =
    match Hashtbl.find global_id_table name with
    | None ->
      let id = FnId.gen () in
      Hashtbl.replace global_id_table ~key:name ~data:id;
      id
    | Some id -> id

  let get_deps (name : name) (inp : ser_input) =
    let id = get_name_id name in
    let dep_info = get_dependency_node id name inp in
    Option.map ~f:(fun r -> r.last_deps |> List.map ~f:(fun (n,_u) ->
      let node = Dag.get n in
      node.name,node.input)) (Dag.get dep_info).cache

  let rec list_any l =
    match l with
    | [] -> false
    | x :: xs -> if x then true else list_any xs

  let dependencies_updated rinfo =
    get_run_id
    >>= fun run_id ->
    if run_id = rinfo.last_run then
      Fiber.return false
    else begin
      rinfo.last_deps |> Fiber.parallel_map ~f:(fun (dep, outp : dep_node * ser_output) ->
        let node = Dag.get dep in
        (* rerun the computation to transitively check / update dependencies *)
        let not_equal =
          Option.map ~f:(fun res ->
            (match res.last_run = run_id with
            | true -> Fiber.return res.last_output
            | false ->
              res.update ())
            >>| (fun cur_outp ->
              match res.cutoff_policy with
              | No_cutoff -> true
              | Cutoff -> outp <> cur_outp
            )
          ) node.cache in
        Option.value ~default:(true |> Fiber.return) not_equal
      )
      (* if any of the dependencies has changed we need to update *)
      >>| (fun dat ->
        let rerun = list_any dat in
        if not rerun then
          rinfo.last_run <- run_id;
        rerun
      )
    end

  let set_running_output_cache kind dep_node fut =
    let di = Dag.get dep_node in
    di.output <- Some (Output.Running fut |> Output.pack kind);
    Fiber.return fut

  let update_caches kind (dep_node : dep_node) out_spec updatefn res =
    get_run_id
    >>= fun run_id ->
      let goinfo = {
        last_deps =
          Dag.children dep_node
          |> List.map ~f:(fun (n : dep_node) ->
            let node = Dag.get n in
            n, (Option.value_exn node.cache).last_output
          );
        last_output = out_spec.serialize res;
        cutoff_policy = out_spec.cutoff_policy;
        update = updatefn;
        last_run = run_id;
      } in
      let di = Dag.get dep_node in
      di.cache <- Some goinfo;
      di.output <- Some (Output.Done res |> Output.pack kind);
      Fiber.return res

  let memoization (name : name) (in_spec : 'a input_spec) (out_spec : 'b output_spec) (comp : 'a -> 'b Fiber.t) : ('a -> 'b Fiber.t) =
    let kind = Kind.create () in
    let fn_id = get_name_id name in

    (* the computation that force computes the fiber *)
    let recompute inp dep_node comp updatefn =
      let di : dep_info = Dag.get dep_node in
      (* create an ivar so other threads can wait for the computation to finish *)
      let ivar : 'b Fiber.Ivar.t = Fiber.Ivar.create () in
      (* create an output cache entry with our ivar *)
      set_running_output_cache kind dep_node ivar
      (* define the function to update / double check intermediate result *)
      (* set context of computation then run it *)
      >>= (fun _ ->
        comp inp |> wrap_fiber { dep_node } di.id)
      (* update the output cache with the correct value *)
      >>= update_caches kind dep_node out_spec updatefn
      (* fill the ivar for any waiting threads *)
      >>= (fun res -> Fiber.Ivar.fill ivar res >>= fun _ -> Fiber.return res) in

    (* the function has already been calculated, determine if
       any inputs have been updated otherwise return the cached
       value *)
    let cached_computation inp (dep_node : dep_node) ginfo res updatefn =
      dependencies_updated ginfo
      >>= (fun updated ->
        if updated then
          recompute inp dep_node comp updatefn
        else
          Fiber.return res
      ) in

    (* determine if the function is still executing *)
    let caching_computation inp (dep_node : dep_node) rinfo updatefn =
      let di = Dag.get dep_node in
      match rinfo with
      | Output.Running fut ->
        Fiber.Ivar.read fut
      | Output.Done res ->
        let ginfo = di.cache |> Option.value_exn in
        cached_computation inp dep_node ginfo res updatefn in

    (* determine if there is an output cache entry *)
    fun inp ->
      let ser_inp = in_spec.serialize inp in
      let dep_info = get_dependency_node fn_id name ser_inp in
      let rec loop () =
        Fiber.return inp
        >>= (fun inp ->
          (* generate the function which recomputes the memoized function
            to ensure it is up to date *)
          let updatefn =
            loop ()
            |> (fun a () -> a >>| out_spec.serialize) in
          (Dag.get dep_info).output |> Option.map ~f:(Output.unpack kind)
          |> (function
              | None -> recompute inp dep_info comp updatefn
              | Some rinfo -> caching_computation inp dep_info rinfo updatefn)
        ) in
      add_rev_dep dep_info >>=
      loop
end
