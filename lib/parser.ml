open Core

let error message: ('a, string) result =
  let message = (
    "Squishy bananas! " ^
    message
  ) in
  Error message

let parse_error kind s_expr: ('a, string) result =
  let s_expr = Sexp.to_string s_expr in
  error ("Unable to parse " ^ kind ^ " `" ^ s_expr ^ "`")

let parse_expr_error expr =
  parse_error "expr" expr

let parse_decl_error decl =
  parse_error "decl" decl

let parse_field_error field =
  parse_error "field" field

let parse_type_error type_ =
  parse_error "type" type_

let from_s_expr input: (Ast.expr, string) result =

  let is_number str =
    String.for_all ~f:Char.is_digit str
  in

  let is_ident str =
    String.for_all ~f:Char.is_alpha str
  in

  let rec is_type s_expr =
    let is_field field =
      match (field: Sexp.t) with
      | List [
        Atom ident;
        field_type;
      ] when is_ident ident && is_type field_type ->
        true
      | _ -> false
    in

    match s_expr with
    | Atom "number"
    | Atom "bool"
    | Atom "void" -> true
    | List [
      Atom "->";
      param_type;
      return_type;
    ] ->
      is_type param_type && is_type return_type
    | List (
      Atom "object" ::
      fields
    ) ->
      List.for_all fields ~f:is_field
    | _ -> false
  in

  let rec parse_type s_expr: (Type.t, string) result =
    let open Result.Monad_infix in

    let parse_field field: (string * Type.t, string) result =
      match (field: Sexp.t) with
      | List [
        Atom ident;
        field_type;
      ] when is_ident ident && is_type field_type ->
        parse_type field_type >>= fun (parsed_field_type) ->
        Ok (ident, parsed_field_type)
      | _ -> parse_field_error field
    in

    match s_expr with
    | Atom "number" -> Ok Type.Num
    | Atom "bool" -> Ok Type.Bool
    | Atom "void" -> Ok Type.Void
    | List [
      Atom "->";
      param_type;
      return_type;
    ] ->
      parse_type param_type >>= fun (parsed_param_type) ->
      parse_type return_type >>= fun (parsed_return_type) ->
      let fn_type = Type.Fn (
        parsed_param_type,
        parsed_return_type
      ) in
      Ok fn_type
    | List (
      Atom "object" ::
      fields
    ) ->
      fields
      |> List.map ~f:parse_field
      |> Result.all
      >>= fun (fields) ->
      let object_type = Type.Object fields in
      Ok object_type
    | _ -> parse_type_error s_expr
  in

  let is_op ~ops input_op =
    List.exists ~f:(
      fun (op) -> String.compare input_op op = 0
    ) ops
  in

  let is_math_op input_op =
    let ops = ["+"; "-"; "*"; "/"] in
    is_op ~ops input_op
  in

  let parse_math_op input_op =
    match input_op with
    | "+" -> Ast.Add
    | "-" -> Ast.Subtract
    | "*" -> Ast.Multiply
    | "/" -> Ast.Divide
    | _ -> failwith ("Invalid math op '" ^ input_op ^ "'")
  in

  let is_compare_op input_op =
    let ops = ["="; "<"] in
    is_op ~ops input_op
  in

  let parse_compare_op input_op =
    match input_op with
    | "=" -> Ast.Equal
    | "<" -> Ast.LessThan
    | _ -> failwith ("Invalid compare op '" ^ input_op ^ "'")
  in

  let is_logic_op input_op =
    let ops = ["and"; "or"] in
    is_op ~ops input_op
  in

  let parse_logic_op input_op =
    match input_op with
    | "and" -> Ast.And
    | "or" -> Ast.Or
    | _ -> failwith ("Invalid logic op '" ^ input_op ^ "'")
  in

  let rec parse_expr input: (Ast.expr, string) result =
    let open Result.Monad_infix in
    match (input: Sexp.t) with
    | Atom "true" -> Ok (Ast.Boolean true)
    | Atom "false" -> Ok (Ast.Boolean false)
    | Atom value when is_number value ->
      let num = int_of_string value in
      Ok (Ast.Number num)
    | Atom ident when is_ident ident ->
      Ok (Ast.Ident ident)
    | List [
      Atom op;
      lhs;
      rhs;
    ] when is_math_op op ->
      parse_expr lhs >>= fun (parsed_lhs) ->
      parse_expr rhs >>= fun (parsed_rhs) ->
      let parsed_op = parse_math_op op in
      let ast = Ast.BinaryMath (
        parsed_lhs,
        parsed_op,
        parsed_rhs
      ) in
      Ok ast
    | List [
      Atom op;
      lhs;
      rhs;
    ] when is_compare_op op ->
      parse_expr lhs >>= fun (parsed_lhs) ->
      parse_expr rhs >>= fun (parsed_rhs) ->
      let parsed_op = parse_compare_op op in
      let ast = Ast.BinaryCompare (
        parsed_lhs,
        parsed_op,
        parsed_rhs
      ) in
      Ok ast
    | List [
      Atom op;
      lhs;
      rhs;
    ] when is_logic_op op ->
      parse_expr lhs >>= fun (parsed_lhs) ->
      parse_expr rhs >>= fun (parsed_rhs) ->
      let parsed_op = parse_logic_op op in
      let ast = Ast.BinaryLogic (
        parsed_lhs,
        parsed_op,
        parsed_rhs
      ) in
      Ok ast
    | List [
      Atom "not";
      rhs;
    ] ->
      parse_expr rhs >>= fun (parsed_rhs) ->
      Ok (Ast.Not parsed_rhs)
    | List [
      Atom "if";
      cond;
      then_;
      else_;
    ] ->
      parse_expr cond >>= fun (parsed_cond) ->
      parse_expr then_ >>= fun (parsed_then) ->
      parse_expr else_ >>= fun (parsed_else) ->
      let ast = Ast.If (
        parsed_cond,
        parsed_then,
        parsed_else
      ) in
     Ok ast
    | List [
      Atom "let";
      List decls;
      body
    ] ->
      parse_decls decls >>= fun (parsed_decls) ->
      parse_expr body >>= fun (parsed_body) ->
      let ast = Ast.Let (parsed_decls, parsed_body) in
      Ok ast
    | List [
      Atom "fun";
      List [
        Atom ident;
        Atom ":";
        ident_type;
      ];
      body
    ] when is_ident ident && is_type ident_type ->
      parse_type ident_type >>= fun (parsed_ident_type) ->
      parse_expr body >>= fun (parsed_body) ->
      let fn_expr = Ast.FnExpr (
        ident,
        parsed_ident_type,
        parsed_body
      ) in
      Ok fn_expr
    | List [
      Atom "app";
      fn_expr;
      fn_arg;
    ] ->
      parse_expr fn_expr >>= fun (parsed_fn_expr) ->
      parse_expr fn_arg >>= fun (parsed_fn_arg) ->
      let fn_app_expr = Ast.FnApp (
        parsed_fn_expr,
        parsed_fn_arg
      ) in
      Ok fn_app_expr
    | List [
      Atom "begin";
      first_expr;
      second_expr;
    ] ->
      parse_expr first_expr >>= fun (parsed_first_expr) ->
      parse_expr second_expr >>= fun (parsed_second_expr) ->
      let begin_expr = Ast.Sequence (
        parsed_first_expr,
        parsed_second_expr
      ) in
      Ok begin_expr
    | expr -> parse_expr_error expr
  and parse_decls decls =
    let open Result.Monad_infix in
    decls
    |> List.map ~f:(fun (decl) ->
      match (decl: Sexp.t) with
      | List [
        Atom ident;
        expr;
      ] ->
        parse_expr expr >>= fun (parsed_expr) ->
        Ok (ident, parsed_expr)
      | decl -> parse_decl_error decl
    )
    |> Result.all
  in

  let input = String.substr_replace_all ~pattern:"[" ~with_:"(" input in
  let input = String.substr_replace_all ~pattern:"]" ~with_:")" input in
  let input = Sexp.of_string input in
  let ast = parse_expr input in
  ast