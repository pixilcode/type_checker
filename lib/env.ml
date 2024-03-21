open Core

type ident = string
type value = Type.t
type table = (ident, value) Hashtbl.t
type t =
  | Top of table
  | Child of table * t

let empty () = Top (Hashtbl.create (module String))

let rec copy parent =
  match parent with
  | Top table -> Top (Hashtbl.copy table)
  | Child (table, parent) -> Child (
    Hashtbl.copy table,
    copy parent
  )

let with_parent parent = Child (Hashtbl.create (module String), parent)


let rec get ~ident env = 
  match env with
  | Top table -> Hashtbl.find table ident
  | Child (table, parent) ->
    match Hashtbl.find table ident with
    | Some value -> Some value
    | None -> get ~ident parent 

let set ~ident ~value env =
  match env with
  | Top table ->
    Hashtbl.set ~key:ident ~data:value table;
    Top table
  | Child (table, parent) ->
    Hashtbl.set ~key:ident ~data:value table;
    Child (table, parent)