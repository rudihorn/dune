open !Stdune

type name = Memoization.name

type input = Dune_lang.Ast.t
type output = Dune_lang.t

type comp = input -> output Fiber.t

type t = {
  functions : (Memoization.name, comp) Hashtbl.t
}

let create () = {
  functions = Hashtbl.create 256
}

let register cs ~key:name ~comp:comp =
  Hashtbl.replace cs.functions ~key:name ~data:comp

let get cs ~key:name =
  Hashtbl.find cs.functions name

