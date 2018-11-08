open Stdune
open Memoization
open Memoization_specs

module Id = Id.Make ()

let id_input_spec =
  let ser x = Id.to_int x |> string_of_int in
  let ne x y = x <> y in
  {
    serialize = ser;
    print = ser;
    not_equal = ne;
  }


let cached () =
  let in_spec = Memoization_specs.map ~f:(fun (i,_) -> i) id_input_spec in
  let comp (_id,fn) = fn in
  Memoize.memoization "cached" in_spec ignore_output_spec comp

let cache cached fn =
  cached (Id.gen (), fn)


