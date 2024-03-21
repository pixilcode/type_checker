open Core
open Sexplib

let from_s_expr input: (Ast.expr, string) result =
  let error message: ('a, string) result =
    let message = (
      "Squishy bananas! " ^
      message
    ) in
    Error message
  in

  let parse_error expr: ('a, string) result =
    let expr = Sexp.to_string expr in
    error ("Unable to parse `" ^ expr ^ "`")
  in

  let is_number str =
    String.for_all ~f:Char.is_digit str
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
    let open Sexp in
    let open Result.Monad_infix in
    match input with
    | Atom "true" -> Ok (Ast.Boolean true)
    | Atom "false" -> Ok (Ast.Boolean false)
    | Atom value when is_number value ->
      let num = int_of_string value in
      Ok (Ast.Number num)
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
    | expr -> parse_error expr
  in

  let input = Sexp.of_string input in
  let ast = parse_expr input in
  ast

let a = Sexp.of_string "(hi)"