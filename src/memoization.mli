open !Stdune

type name = string
type ser_input = string
type ser_output = string

type 'a input_spec = {
  serialize : 'a -> ser_input;
  print : 'a -> string;
  not_equal : 'a -> 'a -> bool;
}

val string_input_spec : string input_spec;;

type 'a output_spec = {
  serialize : 'a -> ser_output;
  print : 'a -> string;
  default_compare : bool option;
}

val string_output_spec : string output_spec;;
val eager_function_output_spec : 'a output_spec;;
val lazy_function_output_spec : 'a output_spec;;


module Memoize : sig
  val memoization : name -> 'a input_spec -> 'b output_spec -> ('a -> 'b Fiber.t) -> ('a -> 'b Fiber.t)

  val get_deps : name -> ser_input -> (name * ser_input) list option

  (* add a dependency to the current fiber context *)
  val add_dep : name -> ser_input -> 'a -> 'a Fiber.t
end

