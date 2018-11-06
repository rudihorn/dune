
type 'a t = {
  mutable value : 'a option;
  eval : unit -> 'a;
}

let make f = {
  value = None;
  eval = f;
}

let recompute o =
  let res = o.eval () in
  o.value <- Some res;
  res

let get o =
  match o.value with
  | None -> recompute o
  | Some r -> r

let return v =
  make (fun () -> v)
