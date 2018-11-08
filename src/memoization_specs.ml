open Memoization
open !Stdune

let id v = v

let show pp v =
  Format.asprintf "%a" pp v

let map ~f (input_spec : 'a input_spec) =
  let ser x = f x |> input_spec.serialize in
  let print x = f x |> input_spec.print in
  let ne x y = input_spec.not_equal (f x) (f y) in
  {
    serialize = ser;
    print = print;
    not_equal = ne;
  }

let string_input_spec = {
  serialize = id;
  print = id;
  not_equal = fun s1 s2 -> s1 <> s2;
}

let pair_l_input_spec (s1 : 'a input_spec) _s2 = {
  not_equal = (fun (v1,_) (v2,_) -> s1.not_equal v1 v2);
  serialize = (fun (a,_) -> s1.serialize a);
  print = (fun (a,_) -> s1.print a);
}

let pair_r_input_spec _s1 (s2 : 'a input_spec) = {
  not_equal = (fun (_,v1) (_,v2) -> s2.not_equal v1 v2);
  serialize = (fun (_,b) -> s2.serialize b);
  print = (fun (_,b) -> s2.print b);
}

let path_input_spec = {
  serialize = (show Path.pp);
  print = (show Path.pp);
  not_equal = fun s1 s2 -> s1 <> s2;
}

let dummy_input_spec = {
  serialize = (fun _ -> "");
  print = (fun _ -> "dummy");
  not_equal = fun _ _ -> true;
}

let int_input_spec = {
  serialize = string_of_int;
  print = string_of_int;
  not_equal = fun i1 i2 -> i1 <> i2;
}

let int_output_spec = {
  serialize = string_of_int;
  print = string_of_int;
  cutoff_policy = Cutoff;
}

let string_output_spec = {
  serialize = id;
  print = id;
  cutoff_policy = Cutoff;
}
let rfn _ =  "<fun>"
let eager_function_output_spec = {
  serialize = rfn;
  print = rfn;
  cutoff_policy = No_cutoff;
}

let empty_string _ = "" 
let ignore_output_spec = {
  Memoization.
  serialize = empty_string;
  print = empty_string;
  cutoff_policy = Cutoff;
}

