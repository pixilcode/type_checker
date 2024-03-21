open Core
open Type_checker

let run_test_case (case_name, case_input) =
  print_endline ("--- " ^ case_name ^ " ---");

  let result = 
    case_input
    |> Parser.from_s_expr
    |> Result.bind ~f:Checker.check
    |> Result.map ~f:Printer.type_to_string
    |> Result.map_error ~f:Printer.error_to_string
  in
  match result with
  | Ok output | Error output ->
    print_endline output;
    print_endline ""
  

let test_cases = [
  ("number", "565");
  ("boolean_1", "true");
  ("boolean_2", "false");

]

let () = List.iter ~f:run_test_case test_cases