signature TRANSACTION =
sig
  type t
  val empty: t
  val insert: int * string * t -> t
  val delete: int * int * t -> t
  val toString: t -> string
  val txns : (int * int * string) vector
end
