open !Stdune

type t
type name = Memoization.name

type input = Dune_lang.Ast.t
type output = Dune_lang.t

type comp = input -> output Fiber.t

val create : unit -> t

val register : t -> key:name -> comp:comp -> unit

val get : t -> key:name -> comp option
