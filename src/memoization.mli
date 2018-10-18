open !Stdune

type ser_input = string
type ser_output = string
type name = string

type 'a input_spec = {
  serialize : 'a -> ser_input;
  print : 'a -> string;
  not_equal : 'a -> 'a -> bool;
}

val string_input_spec : string input_spec
val int_input_spec : int input_spec
val dummy_input_spec : 'a input_spec
val path_input_spec : Path.t input_spec
val pair_l_input_spec : 'a input_spec -> 'b input_spec -> ('a * 'b) input_spec
val pair_r_input_spec : 'a input_spec -> 'b input_spec -> ('a * 'b) input_spec

type compare_policy =
  | Output
  | Recompute


type 'a output_spec = {
  serialize : 'a -> ser_output;
  print : 'a -> string;
  default_compare : compare_policy;
}

val int_output_spec : int output_spec;;
val string_output_spec : string output_spec;;
val eager_function_output_spec : 'a output_spec;;

module CRef : sig 
  type ('a, 'b) t = 'a -> 'b Fiber.t

  type ('a, 'b) comp

  val deferred : unit -> ('a, 'b) comp
  val set : ('a, 'b) comp -> ('a, 'b) t -> unit
  val get : ('a, 'b) comp -> ('a, 'b) t
end

module Memoize : sig
  type 'a t 

  val create_cache : unit -> 'a t

  val memoization : 'b t -> name -> 'a input_spec -> 'b output_spec -> ('a -> 'b Fiber.t) -> ('a -> 'b Fiber.t)

  val get_deps : name -> ser_input -> (name * ser_input) list option

  (* add a dependency to the current fiber context *)
  val add_dep : name -> ser_input -> 'a -> 'a Fiber.t

  val dump_stack : 'a -> 'a Fiber.t

  val get_call_stack : (name * ser_input) list Fiber.t
end

