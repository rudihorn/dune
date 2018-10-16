open !Stdune
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


type compare_policy =
  | Output
  | Recompute

type 'a output_spec = {
  serialize : 'a -> ser_output;
  print : 'a -> string;
  default_compare : compare_policy;
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

type global_cache_info = {
  last_output : ser_output;
  default_compare : compare_policy;
  last_deps : (name * ser_input * ser_output) list;
}

let global_cache_table : (name * ser_input, global_cache_info) Hashtbl.t = Hashtbl.create 256


module Memoize = struct

  type 'output output_cache = {
    last_output : 'output;
  }

  type 'out t = {
    cache : (name * ser_input, 'out output_cache) Hashtbl.t
  }

  let dep_key = Fiber.Var.create ()
  let wrap_fiber (f : 'a Fiber.t) =
    Fiber.Var.set dep_key (ref []) (f >>= fun res -> Fiber.Var.get_exn dep_key >>= (fun deps -> Fiber.return (res,!deps))) 

  let add_dep (dep : string) (inp : ser_input) x =
    Fiber.Var.get dep_key >>= (fun deps -> Option.iter deps ~f:(fun deps -> deps := (dep,inp) :: !deps); (Fiber.return x))

  let last_global_cache (name : name) (inp : ser_input) =
    Hashtbl.find global_cache_table (name, inp)

  let last_output_cache (v : 'b t) (name : name) (inp : ser_input) =
    Hashtbl.find v.cache (name, inp)

  let update_cache (v : 'b t) (name : name) (inp : ser_input) (rinfo : 'b output_cache) =
    Hashtbl.replace v.cache ~key:(name, inp) ~data:rinfo

  let update_global_cache (name : name) (inp : ser_input) (outp : global_cache_info) =
    Hashtbl.replace global_cache_table ~key:(name, inp) ~data:outp

  let get_deps (name : name) (inp : ser_input) =
    let c = last_global_cache name inp in
    Option.map ~f:(fun r -> r.last_deps |> List.map ~f:(fun (n,i,_o) -> n,i)) c

  let dependencies_updated rinfo =
    List.exists ~f:(fun (dep, inp, outp) ->
      let last_res = last_global_cache dep inp in
      let not_equal =
        Option.map ~f:(fun res ->
          match res.default_compare with
          | Recompute -> true
          | Output -> outp <> res.last_output
        ) last_res in
      Option.value ~default:true not_equal
    ) rinfo.last_deps

  let last_global_output_exn (name : name) (inp : ser_input) =
    last_global_cache name inp |> Option.value_exn |> (fun r -> r.last_output)

  let create_cache () =
    {
      cache = Hashtbl.create 256;
    } 

  let memoization (cache : 'b t) (name : name) (in_spec : 'a input_spec) (out_spec : 'b output_spec) (comp : 'a -> 'b Fiber.t) : ('a -> 'b Fiber.t) =
    (fun inp -> 
      let ser_inp = in_spec.serialize inp in
      Fiber.return inp >>= add_dep name ser_inp >>= (fun inp -> 
        let comp = comp inp |> wrap_fiber in
        let recompute =
          comp >>= (fun (res, deps) ->
            let rinfo = {
              last_output = res;
            } in
            update_cache cache name ser_inp rinfo;
            let goinfo = {
              last_deps = List.map ~f:(fun (d,i) -> d, i, last_global_output_exn d i) deps;
              last_output = out_spec.serialize res;
              default_compare = out_spec.default_compare;
            } in
            update_global_cache name ser_inp goinfo;
            Fiber.return res
          ) in
        let rinfo = last_global_cache name ser_inp in
        match rinfo with
        | None -> recompute
        | Some rinfo ->
            if dependencies_updated rinfo then
              recompute 
            else
              (* TODO: recompute rather than value_exn *)
              let lcache = last_output_cache cache name ser_inp |> Option.value_exn in
              Fiber.return lcache.last_output
      )
    )
end

(*
let _ = Path.set_root (Path.External.cwd ())
let _ = Path.set_build_dir (Path.Kind.of_string "_build")

let file_tree = Path.of_string "/home/rhorn/Documents/functors" |> File_tree.load 

let _ = Format.printf "%s" (show File_tree.pp file_tree)

(* let file_tree = File_tree.load Path.root

let _ = Path.External.of_string "/home/rhorn/Documents/functors" |> Path.set_root *)

let stp = Main.setup () |> Fiber.run


let conf = Dune_load.load ()

let _gr = Gen_rules.gen ~contexts:stp.contexts ~build_system:stp.build_system conf

*)
