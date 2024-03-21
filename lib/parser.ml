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
      let ast = Ast.BinaryMath (
        parsed_lhs,
        Ast.Add,
        parsed_rhs
      ) in
      Ok ast
    | expr -> parse_error expr
  in

  let input = Sexp.of_string input in
  let ast = parse_expr input in
  ast

let a = Sexp.of_string "(hi)"