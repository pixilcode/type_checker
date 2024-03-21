open Core

let rec check ast ~env : (Type.t * Env.t, string) result =
  let error message: ('a, string) result =
    let message = (
      "Squishy bananas! " ^
      message
    ) in
    Error message
  in

  let open Result.Monad_infix in
  match ast with
  | Ast.Number _ -> Ok (Type.Num, env)
  | Ast.Boolean _ -> Ok (Type.Bool, env)
  | Ast.BinaryMath (lhs, _, rhs) -> begin
    check lhs ~env >>= fun (lhs_type, env) ->
    check rhs ~env >>= fun (rhs_type, env) ->
    match (lhs_type, rhs_type) with
    | (Type.Num, Type.Num) -> Ok (Type.Num, env)
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
    check lhs ~env >>= fun (lhs_type, env) ->
    check rhs ~env >>= fun (rhs_type, env) ->
    match (lhs_type, rhs_type) with
    | (Type.Num, Type.Num) -> Ok (Type.Bool, env)
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
    check lhs ~env >>= fun (lhs_type, env) ->
    check rhs ~env >>= fun (rhs_type, env) ->
    match (lhs_type, rhs_type) with
    | (Type.Bool, Type.Bool) -> Ok (Type.Bool, env)
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
    check rhs ~env >>= fun (rhs_type, env) ->
    match rhs_type with
    | Type.Bool -> Ok (Type.Bool, env)
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
    check cond ~env >>= fun (cond_type, env) ->
    check then_ ~env >>= fun (then_type, env) ->
    check else_ ~env >>= fun (else_type, env) ->
    match cond_type with
    | Type.Bool ->
      if Type.equal' then_type else_type then
        Ok (then_type, env)
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
  | Ast.Let (decls, body) -> begin
    decls
    |> List.fold_result ~init:env ~f:(fun env decl -> check_decl ~env decl)
    >>= fun (env) ->
      check body ~env
  end
  | Ast.Ident ident -> begin
    let ident_type = Env.get ~ident env in
    match ident_type with
    | Some ident_type -> Ok (ident_type, env)
    | None ->
      let message =
        Printf.sprintf
          "Undefined identifier '%s'"
          ident
      in
      error message
  end
  | Ast.FnExpr (ident, ident_type, body) ->
    let fn_env =
      env
      |> Env.copy
      |> Env.set ~ident ~value:ident_type
    in
    check body ~env:fn_env >>= fun (body_type, _body_env) ->
    let fn_expr_type = Type.Fn (ident_type, body_type) in
    Ok (fn_expr_type, env)
  | Ast.FnApp (fn_expr, fn_arg) -> begin
    check fn_expr ~env >>= fun (fn_expr_type, env) ->
    check fn_arg ~env >>= fun (fn_arg_type, env) ->
    match fn_expr_type with
    | Type.Fn (param_type, return_type) ->
      if Type.equal' param_type fn_arg_type then
        Ok (return_type, env)
      else
        let message =
          Printf.sprintf
            "Function expected arg of type `%s`, found arg of type `%s`"
            (Printer.type_to_string param_type)
            (Printer.type_to_string fn_arg_type)
        in
        error message
    | _ ->
      let message =
        Printf.sprintf
          "Type `%s` cannot be applied as a function"
          (Printer.type_to_string fn_expr_type)
      in
      error message
  end
  | Ast.Sequence (first_expr, second_expr) ->
    let first_expr_env = Env.copy env in
    check first_expr ~env:first_expr_env >>= fun (_, _) ->
    check second_expr ~env
  | Ast.Mutate (ident, expr) -> begin
    check expr ~env >>= fun (expr_type, _env) ->
    let expected_type = Env.get ~ident env in
    match expected_type with
    | Some expected_type -> 
      if Type.equal' expected_type expr_type then
        Ok (Type.Void, env)
      else
        let message =
          Printf.sprintf
            "Assignment to `%s` expected type `%s` but received type `%s`"
            ident
            (Printer.type_to_string expected_type)
            (Printer.type_to_string expr_type)
        in
        error message
    | None ->
      let message =
        Printf.sprintf
          "`%s` is undefined"
          ident
      in
      error message
  end
  | _ -> failwith "unimplemented"
and check_decl (ident, expr) ~env =
  let open Result.Monad_infix in
  check expr ~env >>= fun (expr_type, env) ->
  let env = Env.set ~ident ~value: expr_type env in
  Ok env
