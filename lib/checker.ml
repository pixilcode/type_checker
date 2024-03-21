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
  | Ast.BinaryLogic (lhs, _, rhs) -> begin
    check lhs >>= fun (lhs_type) ->
    check rhs >>= fun (rhs_type) ->
    match (lhs_type, rhs_type) with
    | (Type.Bool, Type.Bool) -> Ok (Type.Bool)
    | _ ->
      let lhs_type_string = Printer.type_to_string lhs_type in
      let rhs_type_string = Printer.type_to_string rhs_type in
      let message =
        Printf.sprintf
          "Invalid types %s and %s for logical operator"
          lhs_type_string
          rhs_type_string
      in
      error message
  end
  | Ast.Not rhs -> begin
    check rhs >>= fun (rhs_type) ->
    match rhs_type with
    | Type.Bool -> Ok (Type.Bool)
    | _ ->
      let rhs_type_string = Printer.type_to_string rhs_type in
      let message =
        Printf.sprintf
          "Invalid type %s for `not` operator"
          rhs_type_string
      in
      error message
  end
  | Ast.If (cond, then_, else_) -> begin
    check cond >>= fun (cond_type) ->
    check then_ >>= fun (then_type) ->
    check else_ >>= fun (else_type) ->
    match cond_type with
    | Type.Bool ->
      if Type.equal' then_type else_type then
        Ok then_type
      else
        error "Branches of if expression should be the same!"
    | _ ->
      let cond_type_string = Printer.type_to_string cond_type in
      let message =
        Printf.sprintf
          "Invalid type %s for if condition"
          cond_type_string
      in
      error message
  end
  | _ -> failwith "unimplemented"
