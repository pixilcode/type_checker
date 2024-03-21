open Core

let rec check ast: (Type.t, string) result =
  let error message: ('a, string) result =
    let message = (
      "Squishy bananas! " ^
      message
    ) in
    Error message
  in

  let open Result.Monad_infix in
  match ast with
  | Ast.Number _ -> Ok Type.Num
  | Ast.Boolean _ -> Ok Type.Bool
  | Ast.BinaryMath (lhs, _, rhs) -> begin
    check lhs >>= fun (lhs_type) ->
    check rhs >>= fun (rhs_type) ->
    match (lhs_type, rhs_type) with
    | (Type.Num, Type.Num) -> Ok (Type.Num)
    | _ ->
      let lhs_type_string = Printer.type_to_string lhs_type in
      let rhs_type_string = Printer.type_to_string rhs_type in
      let message =
        Printf.sprintf
          "Invalid types %s and %s for arithmetic operator"
          lhs_type_string
          rhs_type_string
      in
      error message
  end
  | Ast.BinaryCompare (lhs, _, rhs) -> begin
    check lhs >>= fun (lhs_type) ->
    check rhs >>= fun (rhs_type) ->
    match (lhs_type, rhs_type) with
    | (Type.Num, Type.Num) -> Ok (Type.Bool)
    | _ ->
      let lhs_type_string = Printer.type_to_string lhs_type in
      let rhs_type_string = Printer.type_to_string rhs_type in
      let message =
        Printf.sprintf
          "Invalid types %s and %s for comparison operator"
          lhs_type_string
          rhs_type_string
      in
      error message
  end
  | _ -> failwith "unimplemented"
