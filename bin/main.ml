open Type_checker
open Core

let () =
  let open Result.Monad_infix in

  (* read stdin as a single string *)
  In_channel.input_all In_channel.stdin

  (* parse the input *)
  |> Parser.from_s_expr

  (* type check the input *)
  >>= Checker.check ~env:(Env.empty ())

  (* get the result type *)
  >>| fst

  (* convert the type to a string *)
  >>| Printer.type_to_string

  (* convert any errors to a string *)
  |> Result.map_error ~f:Printer.error_to_string

  (* print the result *)
  |> function
  | Ok output
  | Error output -> print_endline output

