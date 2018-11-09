(** An environment node represents an evaluated (env ..) stanza in a
    directory. *)

open Stdune

type t

val make
  :  dir:Path.t
  -> inherit_from:t Lazy.t option
  -> scope:Scope.t
  -> config:Dune_env.Stanza.t
  -> env:Env.t option
  -> t

val scope : t -> Scope.t

val external_ : t -> profile:string -> default:Env.t -> Env.t

val ocaml_flags : t -> profile:string -> expander:Expander.t -> Ocaml_flags.t
