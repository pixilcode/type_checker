open Core
open Sexplib

let from_s_expr input: (Ast.expr, string) result =
  let error message: ('a, string) result =
    let message = (
      "Squishy banana! " ^
      message
    ) in
    Error message
  in

  let is_number str =
    String.for_all ~f:Char.is_digit str
  in
  
  let parse_expr input: (Ast.expr, string) result =
    let open Sexp in
    match input with
    | Atom "true" -> Ok (Ast.Boolean true)
    | Atom "false" -> Ok (Ast.Boolean false)
    | Atom value when is_number value ->
      let num = int_of_string value in
      Ok (Ast.Number num)
    | expr ->
      let expr = Sexp.to_string expr in
      error ("Unable to parse `" ^ expr ^ "`")
  in

  let input = Sexp.of_string input in
  let ast = parse_expr input in
  ast

let a = Sexp.of_string "(hi)"