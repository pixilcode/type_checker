open Core
open Type_checker

let run_test_case (case_name, case_input) =
  let open Result.Monad_infix in
  print_endline ("--- " ^ case_name ^ " ---");

  let env = Env.empty () in

  let result = 
    case_input
    |> Parser.from_s_expr
    >>= Checker.check ~env
    >>| fst
    >>| Printer.type_to_string
    |> Result.map_error ~f:Printer.error_to_string
  in
  match result with
  | Ok output | Error output ->
    print_endline output;
    print_endline ""
  

let test_cases = [
  ("number", "565");
  ("boolean_1", "#true");
  ("boolean_2", "#false");
  ("plus", "(+ 1 2)");
  ("minus", "(- 1 2)");
  ("times", "(* 1 2)");
  ("divide", "(/ 1 2)");
  ("equal", "(= 1 2)");
  ("less_than", "(< 1 2)");
  ("and", "(and #true #false)");
  ("or", "(or #true #false)");
  ("not", "(not #true)");
  ("if", "(if #true 1 2)");
  ("let", "(let ([x 1]) 1)");
  ("let_ident", "(let ([x 1]) x)");
  ("let_multi", "(let ([x 1] [y 2]) (+ x y))");
  ("let_nested", "(let ([x 1]) (let ([y 2]) (+ x y)))");
  ("fn_expr", "(fun (x : num) x)");
  ("fn_app", "(app (fun (x : num) x) 1)");
  ("sequence", "(begin 1 #true)");
  ("mutation", "(let ([x 1]) (set! x 2))");
  ("object", "(object [x 1])");
  ("field_access", "(let ([x (object [y 1])]) (field x y))");
  ("duplicate_fields_error", "(object [x 10] [x 12])");
]

let () = List.iter ~f:run_test_case test_cases