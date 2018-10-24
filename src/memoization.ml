open !Stdune
open Import
(* open Staged *)
open Fiber.O

type name = string

(* seralized input / output types *)
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
  let bf = Buffer.create 256 in
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



type cutoff_policy =
  | No_cutoff 
  | Cutoff 

type 'a output_spec = {
  serialize : 'a -> ser_output;
  print : 'a -> string;
  cutoff_policy : cutoff_policy;
}

let int_output_spec = {
  serialize = string_of_int;
  print = string_of_int;
  cutoff_policy = Cutoff;
}

let string_output_spec = {
  serialize = id;
  print = id;
  cutoff_policy = Cutoff;
}
let rfn _ =  "<fun>"
let eager_function_output_spec = {
  serialize = rfn;
  print = rfn;
  cutoff_policy = No_cutoff;
}

(* a function that will try to recompute the memoized function *)
type update_cache = unit -> ser_output Fiber.t

type 'output run_state =
  | Running of 'output Fiber.Ivar.t
  | Done of 'output

type global_cache_info = {
  last_output : ser_output;
  update : update_cache;
  cutoff_policy : cutoff_policy;
  last_deps : (name * ser_input * ser_output) list;
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

  type stack_frame = {
    name : name;
    input : ser_input;
  }

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

  (* for each run we would like to check if the
     dependency has changed or not by rexecuting it, to
     avoid exponential blow up we keep track of whether a
     dependency has been checked during the current run and
     if so we don't execute it *)
  let checked_key = Fiber.Var.create ()
  let checked_or_check id =
    Fiber.Var.get checked_key
    >>= function
    | None -> assert false
    | Some set ->
      if Id.Set.mem id !set then
        Fiber.return true
      else
        (set := Id.Set.add id !set;
        Fiber.return false)

  (* mark the current node as up to date *)
  let check id v =
    Fiber.Var.get checked_key
    >>| function
    | None -> assert false
    | Some set ->
        set := Id.Set.add id !set;
        v

  let run_memoize fiber =
    Fiber.Var.set checked_key (ref Id.Set.empty) fiber

  (* set up the fiber so that it both has an up to date call stack
     as well as an empty dependency table *)
  let wrap_fiber (stack_frame : stack_frame) id (f : 'a Fiber.t) =
    let dep_ref = ref [] in
    (* transform f so it returns the dependencies after the computation *)
    let f = f >>= fun res -> Fiber.return (res,!dep_ref) in
    (* change f so it sets up a new dependency table before computation *)
    let f = Fiber.Var.set dep_key dep_ref f in
    (* set the context so that f has the call stack  *)
    get_call_stack_int
      >>| (fun (stack, set) -> stack_frame :: stack, Id.Set.add id set) (* add top entry *)
      >>= (fun stack -> Fiber.Var.set call_stack_key stack f) (* update *)

  let dump_stack v =
    get_call_stack
      >>|
        (Printf.printf "Memoized function stack:\n";
        List.iter ~f:(fun st -> Printf.printf "   %s %s\n" st.name st.input))
      >>| (fun _ -> v)

  (*
  let find_cycle name input v =
    get_call_stack
      >>| List.iter ~f:(fun stack_entry -> if stack_entry = (name, input) then failwith "cycle detected")
      >>| (fun _ -> v) *)

  let find_cycle id v =
    get_call_stack_int
      >>| (fun (stack,set) ->
             if Id.Set.mem id set then
               die "Cycle detected:\n    %s"
                 (List.map stack ~f:(fun rule -> Printf.sprintf "%s %s" rule.name rule.input)
                  |> String.concat ~sep:"\n-> "))
      >>| (fun _ -> v)

  let add_dep (dep : string) (inp : ser_input) x =
    (* get_call_stack_int
      >>| (function | [],_ -> "(root)" | x :: _, _ -> x.name ^ " " ^ x.input)
      >>| (fun from -> Printf.printf "%s -> %s %s\n%!" from dep inp)
    >>> *)
    Fiber.Var.get dep_key >>= (fun deps -> Option.iter deps ~f:(fun deps -> deps := (dep,inp) :: !deps); (Fiber.return x))

  let ignore_deps fiber =
    Fiber.Var.set dep_key (ref []) fiber

  let last_global_cache (name : name) (inp : ser_input) =
    Hashtbl.find global_cache_table (name, inp)

  let last_global_cache_exn (name : name) (inp : ser_input) : global_cache_info =
    Hashtbl.find global_cache_table (name, inp) |>
    function
      | Some e -> e
      | None ->
        Printf.printf "No global memoized cache entry for (%s, %s). Did it crash?\n%!" name inp;
        die "Error"

  let last_global_output_exn (name : name) (inp : ser_input) =
      last_global_cache_exn name inp |> (fun r -> r.last_output)

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

  let rec list_any l =
    match l with
    | [] -> false
    | x :: xs -> if x then true else list_any xs

  let dependencies_updated id rinfo =
    rinfo.last_deps |> Fiber.parallel_map ~f:(fun (dep, inp, outp) ->
      (* rerun the computation to transitively check / update dependencies *)
      let last_res = last_global_cache dep inp in
      let not_equal =
        Option.map ~f:(fun res ->
          checked_or_check id
          >>= (function
          | true -> Fiber.return res.last_output
          | false -> res.update ())
          >>| (fun cur_outp ->
            match res.cutoff_policy with
            | No_cutoff -> true
            | Cutoff -> outp <> cur_outp
          )
        ) last_res in
      Option.value ~default:(true |> Fiber.return) not_equal
    )
    (* if any of the dependencies has changed we need to update *)
    >>| (fun dat -> list_any dat)

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

  let update_caches cache name ser_inp out_spec id updatefn (res, deps) =
    let rinfo = {
      state = Done res;
      id = id;
    } in
    let goinfo = {
      last_deps = deps |> List.map ~f:(fun (d,i) ->
        d,i, last_global_output_exn d i);
      last_output = out_spec.serialize res;
      cutoff_policy = out_spec.cutoff_policy;
      update = updatefn;
    } in
    update_global_cache name ser_inp goinfo;
    update_cache cache name ser_inp rinfo;
    Fiber.return res

  let rec memoization (cache : 'b t) (name : name) (in_spec : 'a input_spec) (out_spec : 'b output_spec) (comp : 'a -> 'b Fiber.t) : ('a -> 'b Fiber.t) =
    (* the computation that force computes the fiber *)
    let recompute inp ser_inp id comp updatefn =
      (* create an ivar so other threads can wait for the computation to finish *)
      let ivar : 'b Fiber.Ivar.t = Fiber.Ivar.create () in
      (* create an output cache entry with our ivar *)
      set_running_output_cache cache name ser_inp id ivar
      (* define the function to update / double check intermediate result *)
      (* set context of computation then run it *)
      >>= (fun _ ->
        comp inp |> wrap_fiber { name; input = ser_inp } id)
      >>= check id (* mark the current node as up to date *)
      (* update the output cache with the correct value *)
      >>= update_caches cache name ser_inp out_spec id updatefn
      (* fill the ivar for any waiting threads *)
      >>= (fun res -> Fiber.Ivar.fill ivar res >>= fun _ -> Fiber.return res) in

    (* the function has already been calculated, determine if
       any inputs have been updated otherwise return the cached
       value *)
    let cached_computation inp ser_inp id ginfo res updatefn =
      dependencies_updated id ginfo |> ignore_deps
      >>= (fun updated ->
        if updated then
          recompute inp ser_inp id comp updatefn
        else
          Fiber.return res
      ) in

    (* determine if the function is still executing *)
    let caching_computation inp ser_inp rinfo updatefn =
      match rinfo.state with
      | Running fut ->
        find_cycle rinfo.id inp
        >>= (fun _ -> Fiber.Ivar.read fut)
      | Done res ->
        let ginfo = last_global_cache_exn name ser_inp in
        cached_computation inp ser_inp rinfo.id ginfo res updatefn in

    (* determine if there is an output cache entry *)
    (fun inp ->
      let ser_inp = in_spec.serialize inp in
      Fiber.return inp
      >>= (fun inp ->
        let updatefn =
          inp
          |> memoization cache name in_spec out_spec comp
          |> (fun a () -> a >>| out_spec.serialize) in
        add_dep name ser_inp inp
        >>| (fun _ -> last_output_cache cache name ser_inp)
        >>= (function
            | None -> recompute inp ser_inp (Id.gen ()) comp updatefn
            | Some rinfo -> caching_computation inp ser_inp rinfo updatefn)
      )
    )
end
