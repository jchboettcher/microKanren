open Ast

let rec string_of_exp (u : exp) : string =
  begin match u with
    | Int i -> string_of_int i
    | Var x -> x
    | Pair (u, v) -> "(" ^ (string_of_exp u) ^ "," ^ (string_of_exp v) ^ ")"
  end

let string_of_state ((s, n) : state) : string =
  (List.fold_left (fun acc (x, u) -> acc ^ x ^ " = " ^ (string_of_exp u) ^ "; ") "{ " s) ^ "}  " ^ (string_of_int n)

let string_of_state_option (s : state option) : string =
  begin match s with
    | Some s -> string_of_state s
    | None -> "None"
  end

let string_of_state_stream (str : state stream) (n : int) : string =
  let rec helper (str : state stream) (n : int) : string =
    if (n <= 0 && str != MZero) then "  ...\n" else 
    begin match str with
      | MZero -> ""
      | Fun f -> helper (f ()) n
      | Mat (s, str') -> "  " ^ (string_of_state s) ^ ";\n" ^ (helper str' (n-1))
    end in
  "[\n" ^ (helper str n) ^ "]"

let print_stream (str : state stream) (n : int) : unit =
  Printf.printf "%s\n" (string_of_state_stream str n)
