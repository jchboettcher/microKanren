type var = string

type exp =
  | Int of int
  | Var of var
  | Pair of exp * exp

type state = ((var * exp) list) * int

type 'a stream =
  | MZero
  | Fun of (unit -> ('a stream))
  | Mat of 'a * ('a stream)

type goal = state -> state stream
