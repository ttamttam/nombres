type options =
  | Septante
  | Huitante
  | Nonante
  | Belgique (* Septante Nonante *)
  | VVF (* Septante Huitante Nonante *)

val nombre : ?options:options list -> int -> string
