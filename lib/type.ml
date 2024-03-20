type field = string

type t =
  | Num
  | Bool
  | Void
  | Fn of t * t
  | Object of (field * t) list