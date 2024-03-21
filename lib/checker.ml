open Core

let check ast: (Type.t, string) result =
  match ast with
  | Ast.Number _ -> Ok Type.Num
  | Ast.Boolean _ -> Ok Type.Bool
  | _ -> failwith "unimplemented"
