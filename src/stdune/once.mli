(** Cached lazy evaluation *)

(** Similar to the Lazy module, the Once module allows
    the definition of a value which is only evaluated when
    Once.get is called. Unlike the lazy module, the value
    isn't recomputed each time, but instead of only the first
    time Once.get is called. After that the cached value is
    always returned. *)

type 'a t = {
  mutable value : 'a option;
  eval : unit -> 'a
}

(** Return cached value or compute the value. *)
val get : 'a t -> 'a

(** Force the value to be recomputed. *)
val recompute : 'a t -> 'a

(** [Once.make f] specifies a computation [f] which should
    be evaluated when [Once.get] is called on the result. *)
val make : (unit -> 'a) -> 'a t

(** [Once.return v] uses the static value [v] as the result
    of [Once.get] *)
val return : 'a -> 'a t
