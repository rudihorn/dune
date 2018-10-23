open !Stdune
open Import
(* open Staged *)
open Fiber.O

type name = string
type ser_input = string
type ser_output = string

type 'a input_spec = {
  serialize : 'a -> ser_input;
  print : 'a -> string;
  not_equal : 'a -> 'a -> bool;
}

let id v = v

let string_input_spec = {
  serialize = id;
  print = id;
  not_equal = fun s1 s2 -> s1 <> s2;
}

let show pp v =
  let bf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer bf in
  pp fmt v;
  Format.pp_print_flush fmt ();
  Buffer.contents bf

let pair_l_input_spec s1 _s2 = {
  serialize = (fun (a,_) -> s1.serialize a);
  print = (fun (a,_) -> s1.print a);
  not_equal = fun (s1,_) (s2,_) -> s1 <> s2;
}

let pair_r_input_spec _s1 s2 = {
  serialize = (fun (_,b) -> s2.serialize b);
  print = (fun (_,b) -> s2.print b);
  not_equal = fun (_,s1) (_,s2) -> s1 <> s2;
}

let path_input_spec = {
  serialize = (show Path.pp);
  print = (show Path.pp);
  not_equal = fun s1 s2 -> s1 <> s2;
}

let dummy_input_spec = {
  serialize = (fun _ -> "");
  print = (fun _ -> "dummy");
  not_equal = fun _ _ -> true;
}

let int_input_spec = {
  serialize = string_of_int;
  print = string_of_int;
  not_equal = fun i1 i2 -> i1 <> i2;
}



type compare_policy =
  | Output
  | Recompute

type 'a output_spec = {
  serialize : 'a -> ser_output;
  print : 'a -> string;
  default_compare : compare_policy;
}

let int_output_spec = {
  serialize = string_of_int;
  print = string_of_int;
  default_compare = Output;
}

let string_output_spec = {
  serialize = id;
  print = id;
  default_compare = Output;
}
let rfn _ =  "<fun>"
let eager_function_output_spec = {
  serialize = rfn;
  print = rfn;
  default_compare = Recompute;
}

type update_cache = unit -> ser_output Fiber.t

type 'output run_state =
  | Running of 'output Fiber.Ivar.t
  | Done of 'output

type global_cache_info = {
  last_output : ser_output;
  default_compare : compare_policy;
  last_deps : (name * ser_input * update_cache) list;
}

let global_cache_table : (name * ser_input, global_cache_info) Hashtbl.t = Hashtbl.create 256

module Id = struct
  include IdGen

  let idgen = IdGen.create ()
  let gen () = IdGen.gen idgen 
end


module CRef = struct
  type ('a, 'b) t = 'a -> 'b Fiber.t

  type ('a, 'b) state =
  | Empty 
  | Full of ('a, 'b) t

  type ('a, 'b) comp = {
    mutable state : ('a, 'b) state
  }

  let deferred () = { state = Empty }

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

  type 'output output_cache = {
    state : 'output run_state;
    id : Id.t;
  }

  type 'out t = {
    cache : (name * ser_input, 'out output_cache) Hashtbl.t;
  }

  (* fiber context variable keys *)
  let dep_key = Fiber.Var.create ()
  let call_stack_key = Fiber.Var.create ()
  let get_call_stack_int =
    Fiber.Var.get call_stack_key (* get call stack *)
      >>| Option.value ~default:([], Id.Set.empty) (* default call stack is empty *)

  let get_call_stack =
    get_call_stack_int >>| (fun (stack,_) -> stack)

  (* set up the fiber so that it both has an up to date call stack
     as well as an empty dependency table *)
  let wrap_fiber (add_stack : name * ser_input) id (f : 'a Fiber.t) =
    (* transform f so it returns the dependencies after the computation *)
    let f = f >>= fun res -> Fiber.Var.get_exn dep_key >>= (fun deps -> Fiber.return (res,!deps)) in
    (* change f so it sets up a new dependency table before computation *)
    let f = Fiber.Var.set dep_key (ref []) f in
    (* set the context so that f has the call stack  *)
    get_call_stack_int
      >>| (fun (stack, set) -> add_stack :: stack, Id.Set.add id set) (* add top entry *)
      >>= (fun stack -> Fiber.Var.set call_stack_key stack f) (* update *)

  let dump_stack v =
    get_call_stack
      >>| List.iter ~f:(fun (n,v) -> n ^ " " ^ v |> print_endline)
      >>| (fun _ -> v)

  (*
  let find_cycle name input v =
    get_call_stack
      >>| List.iter ~f:(fun stack_entry -> if stack_entry = (name, input) then failwith "cycle detected")
      >>| (fun _ -> v) *)

  let find_cycle id v =
    get_call_stack_int
      >>| (fun (_stack,set) ->
             if Id.Set.mem id set then
               die "Cycle detected:")
      >>| (fun _ -> v)

  let add_dep (dep : string) (inp : ser_input) (update : update_cache) x =
    Fiber.Var.get dep_key >>= (fun deps -> Option.iter deps ~f:(fun deps -> deps := (dep,inp,update) :: !deps); (Fiber.return x))

  let last_global_cache (name : name) (inp : ser_input) =
    Hashtbl.find global_cache_table (name, inp)

  let last_global_cache_exn (name : name) (inp : ser_input) =
    Hashtbl.find global_cache_table (name, inp) |> Option.value_exn 

  let last_output_cache (v : 'b t) (name : name) (inp : ser_input) =
    Hashtbl.find v.cache (name, inp)

  let _last_output_cache_exn (v : 'b t) (name : name) (inp : ser_input) =
    last_output_cache v name inp |> Option.value_exn

  let update_cache (v : 'b t) (name : name) (inp : ser_input) (rinfo : 'b output_cache) =
    Hashtbl.replace v.cache ~key:(name, inp) ~data:rinfo

  let update_global_cache (name : name) (inp : ser_input) (outp : global_cache_info) =
    Hashtbl.replace global_cache_table ~key:(name, inp) ~data:outp

  let get_deps (name : name) (inp : ser_input) =
    let c = last_global_cache name inp in
    Option.map ~f:(fun r -> r.last_deps |> List.map ~f:(fun (n,i,_u) -> n,i)) c

  let dependencies_updated rinfo =
    rinfo.last_deps |> Fiber.parallel_map ~f:(fun (dep, inp, upd) ->
      (* rerun the computation to transitively check / update dependencies *)
      upd ()
      (* compare with last used output *)
      >>| (fun outp -> 
        let last_res = last_global_cache dep inp in
        let not_equal =
          Option.map ~f:(fun res ->
            match res.default_compare with
            | Recompute -> true
            | Output -> outp <> res.last_output
          ) last_res in
        Option.value ~default:true not_equal
      )
    )
    (* if any of the dependencies has changed we need to update *)
    >>| (fun dat -> dat |> List.exists ~f:(fun b -> b))

  let create_cache () =
    {
      cache = Hashtbl.create 256;
    }

  let set_running_output_cache cache name ser_inp id fut =
    let rinfo = {
      state = Running fut;
      id = id;
    } in
    update_cache cache name ser_inp rinfo;
    Fiber.return fut

  let update_caches cache name ser_inp out_spec id (res, deps) =
    let rinfo = {
      state = Done res;
      id = id;
    } in
    update_cache cache name ser_inp rinfo;
    let goinfo = {
      last_deps = deps |> List.map ~f:(fun (d,i,u) -> d, i, u);
      last_output = out_spec.serialize res;
      default_compare = out_spec.default_compare;
    } in
    update_global_cache name ser_inp goinfo;
    Fiber.return res

  let rec memoization (cache : 'b t) (name : name) (in_spec : 'a input_spec) (out_spec : 'b output_spec) (comp : 'a -> 'b Fiber.t) : ('a -> 'b Fiber.t) =
    (* the computation that force computes the fiber *)
    let recompute inp ser_inp id comp =
      (* create an ivar so other threads can wait for the computation to finish *)
      let ivar : 'b Fiber.Ivar.t = Fiber.Ivar.create () in
      (* create an output cache entry with our ivar *)
      set_running_output_cache cache name ser_inp id ivar
      (* define the function to update / double check intermediate result *)
      (* set context of computation then run it *)
      >>= (fun _ ->
        comp inp |> wrap_fiber (name, ser_inp) id)
      (* update the output cache with the correct value *)
      >>= update_caches cache name ser_inp out_spec id
      (* fill the ivar for any waiting threads *)
      >>= (fun res -> Fiber.Ivar.fill ivar res >>= fun _ -> Fiber.return res) in

    (* the function has already been calculated, determine if
       any inputs have been updated otherwise return the cached
       value *)
    let cached_computation inp ser_inp id ginfo res =
      dependencies_updated ginfo
      >>= (fun updated ->
        if updated then
          recompute inp ser_inp id comp
        else
          Fiber.return res
      ) in

    (* determine if the function is still executing *)
    let caching_computation inp ser_inp rinfo =
      match rinfo.state with
      | Running fut ->
        find_cycle rinfo.id inp
        >>= (fun _ -> Fiber.Ivar.read fut)
        (* >>| (fun _ -> last_output_cache_exn cache name ser_inp)
        >>= (fun rinfo -> caching_computation inp ser_inp rinfo) *)
      | Done res ->
        let ginfo = last_global_cache_exn name ser_inp in
        cached_computation inp ser_inp rinfo.id ginfo res in

    (* determine if there is an output cache entry *)
    (fun inp ->
      let ser_inp = in_spec.serialize inp in
      let updatefn = inp |> memoization cache name in_spec out_spec comp |> (fun a () -> a >>| out_spec.serialize) in
      Fiber.return inp
      >>= add_dep name ser_inp updatefn
      >>| (fun _ -> last_output_cache cache name ser_inp)
      >>= (function
           | None -> recompute inp ser_inp (Id.gen ()) comp
           | Some rinfo -> caching_computation inp ser_inp rinfo)
    )
end
