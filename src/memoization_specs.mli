open Memoization
open !Stdune

val string_input_spec : string input_spec
val int_input_spec : int input_spec
val dummy_input_spec : 'a input_spec
val path_input_spec : Path.t input_spec
val pair_l_input_spec : 'a input_spec -> 'b input_spec -> ('a * 'b) input_spec
val pair_r_input_spec : 'a input_spec -> 'b input_spec -> ('a * 'b) input_spec

val int_output_spec : int output_spec;;
val string_output_spec : string output_spec;;
val eager_function_output_spec : 'a output_spec;;
