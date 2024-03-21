type ident = string
type value = Type.t
type t

val empty: unit -> t
val copy: t -> t
val with_parent: t -> t
val get: ident:ident -> t -> value option
val set: ident:ident -> value:value -> t -> t