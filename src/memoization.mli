open !Stdune

(** Serialized input type. *)
type ser_input = string

(** Serialized output type. *)
type ser_output = string

(** Function name type. *)
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

(** The cutoff policy of an output type. *)
type cutoff_policy =
  | No_cutoff (** The output should not be compared and the function should always be recomputed. *)
  | Cutoff (** The outputs should be compared and only recomputed if they differ. *)

type 'a output_spec = {
  serialize : 'a -> ser_output;
  print : 'a -> string;
  cutoff_policy : cutoff_policy;
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

type dep_info
type dep_node

module Memoize : sig
  type 'a t 

  type stack_frame = {
    name : name;
    input : ser_input;
    dep_node : dep_node;
  }

  (** Create a cache used by the memoization for the specific output type.
      Each output type requires its own cache. *)
  val create_cache : unit -> 'a t

  (** Take a computation of the form 'a -> 'b Fiber.t and produced a memoized function
      which returns cached values. The arguments of the memoization function in order are:
        - An output cache where the outputs are stored
        - Name of the function
        - An input type specification, which tells how to serialize and compare the input.
        - An output type specification, which tells how to serialize the output as well as
          what the cutoff policy is
        - The computation, which takes an input type and returns an output type fiber

      The function returns a new fiber of type 'a -> 'b Fiber.t, which automatically
      tries to find the old result and otherwise recomputes it.
  *)
  val memoization : 'b t -> name -> 'a input_spec -> 'b output_spec -> ('a -> 'b Fiber.t) -> ('a -> 'b Fiber.t)

  (** After running a memoization function with a given name and input, it is possibly to query
      which dependencies that function used during execution by calling `get_deps` with the
      name and input used during execution. *)
  val get_deps : name -> ser_input -> (name * ser_input) list option

  (** This function sets up required variables for the execution of a memoized Fiber.
      Example usage:
        let myfiber = memoization ... in
        myfiber myinput |> run_memoize |> Fiber.run
  *)
  val run_memoize : 'a Fiber.t -> 'a Fiber.t

  (** Print the memoized call stack during execution. This is useful for debugging purposes.
      Example code:
        some_fiber_computation
        >>= dump_stack
        >>= some_more_computation
  *)
  val dump_stack : 'a -> 'a Fiber.t

  (** Get the memoized call stack during the execution of a memoized function. *)
  val get_call_stack : stack_frame list Fiber.t
end

