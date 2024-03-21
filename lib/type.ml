type field = string

type t =
  | Num
  | Bool
  | Void
  | Fn of t * t
  | Object of (field * t) list

let rec equal' t1 t2 =
  let open Core in
  match (t1, t2) with
  | (Num, Num)
  | (Bool, Bool)
  | (Void, Void) -> true
  | (Fn (param1, return1), Fn (param2, return2)) ->
    (equal' param1 param2) && (equal' return1 return2)
  | (Object fields1, Object fields2) ->
    List.for_all fields1 ~f:(fun (field1_name, field1_value) ->
      List.exists fields2 ~f:(fun (field2_name, field2_value) -> 
        (String.compare field1_name field2_name) = 0
        && (equal' field1_value field2_value)
      )
    )
  | _ -> false