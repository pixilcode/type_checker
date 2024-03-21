let error_to_string message =
  Printf.sprintf "(error \"%s\")" message

let type_to_string type_ =
  let rec get_type_string type_ =
    match type_ with
    | Type.Num -> "(number)"
    | Type.Bool -> "(boolean)"
    | Type.Void -> "(void)"
    | Type.Fn (param, return) ->
      let param = get_type_string param in
      let return = get_type_string return in
      Printf.sprintf "(-> %s %s)" param return
    | Type.Object fields ->
      let fields =
        fields
        |> List.map (fun (ident, type_) -> 
          let type_ = get_type_string type_ in
          Printf.sprintf "[%s %s]" ident type_
        )
        |> String.concat " "
      in
      Printf.sprintf "(object %s)" fields
  in

  let type_string = get_type_string type_ in
  Printf.sprintf "(type %s)" type_string