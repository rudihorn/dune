type 'a t

val cached : unit -> 'a t

val cache : 'a t -> 'a Fiber.t -> 'a Fiber.t
