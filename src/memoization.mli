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

(** The cutoff policy of an output type. *)
type cutoff_policy =
  | No_cutoff (** The output should not be compared and the function should always be recomputed. *)
  | Cutoff (** The outputs should be compared and only recomputed if they differ. *)

type 'a output_spec = {
  serialize : 'a -> ser_output;
  print : 'a -> string;
  cutoff_policy : cutoff_policy;
}

(** Forward declarations are used to allow the recursive definitions of memoized functions. Similar to IVar's,
    the user can create a forward declaration by calling [create], get it's value with [get] and
    fill the value with [set]. *)
module Fdecl : sig
  type ('a, 'b) t = 'a -> 'b Fiber.t

  type ('a, 'b) comp

  (** [create ()] creates a forward declaration. *)
  val create : unit -> ('a, 'b) comp

  (** [set comp f] set's the value that is returned by [get comp] to [f].
      @raise Exn.Fatal_error if [set] was already called *)
  val set : ('a, 'b) comp -> ('a, 'b) t -> unit

  (** [get comp] returns the [f] if [set comp f] was called.
      @raise Exn.Fatal_error if [set] has not been called yet. *)
  val get : ('a, 'b) comp -> ('a, 'b) t
end

(** A stack frame within a computation. *)
module Stack_frame : sig
  type t

  val name : t -> name
  val input : t -> ser_input
end

module Cycle_error : sig
  type t

  exception E of t

  (** Return the stack leading to the node which raised the cycle. *)
  val stack : t -> Stack_frame.t list

  (** Get the inputs of all entries with the specified name. *)
  val filter : name:name -> t -> ser_input list

  (** Generate a list of strings for each entry in the cycle. *)
  val serialize : t -> string list

  (** Generate a list of strings for each entry in the stack. *)
  val serialize_stack : t -> string list
end

module Memoize : sig
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

      Running the computation may raise [MemCycle] if a cycle is detected.
  *)
  val memoization : name -> 'a input_spec -> 'b output_spec -> ('a -> 'b Fiber.t) -> ('a -> 'b Fiber.t)

  (** After running a memoization function with a given name and input, it is possibly to query
      which dependencies that function used during execution by calling [get_deps] with the
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
  val get_call_stack : Stack_frame.t list Fiber.t
end

