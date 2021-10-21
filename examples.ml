open Ast
open Microkanren

let a_and_b = conj (
  callfresh (fun a -> equal (a, Int 7)),
  callfresh (fun b -> disj (
    equal (b, Int 5),
    equal (b, Int 6)
  ))
)

let rec fives' x = disj (equal (x, Int 5), fun s -> Fun (fun () -> (fives' x) s))
let rec sixes' x = disj (equal (x, Int 6), fun s -> Fun (fun () -> (sixes' x) s))

let fives = callfresh fives'
let fives_sixes = callfresh (fun x -> disj (fives' x, sixes' x))

let rec find_list' x y = disj (equal (x, y),
  callfresh (fun h ->
    callfresh (fun t ->
      conj (
        equal (y, Pair (h, t)),
        fun s -> (
          Fun (fun () -> (
            find_list' x t s
          ))
        )
      )
    )
  )
)

let find_list_x = callfresh (fun x -> find_list' x (Pair (Int 1, Pair (Int 2, Pair (Int 3, Pair (Int 4, Int 5))))))
let find_list_y = callfresh (fun y -> find_list' (Int 1) y)
let find_list_xy = callfresh (fun x -> callfresh (fun y -> find_list' x y))

let rec find_tree' x y = disj (equal (x, y),
  callfresh (fun p1 ->
    callfresh (fun p2 ->
      conj (
        equal (y, Pair (p1, p2)),
        fun s -> (
          Fun (fun () -> (
            disj (
              find_tree' x p1,
              find_tree' x p2
            ) s
          ))
        )
      )
    )
  )
)

let find_tree_x1 = callfresh (fun x -> find_tree' x (Pair (Pair (Int 1, Int 2), Pair (Int 3, Int 4))))
let find_tree_x2 = callfresh (fun x -> find_tree' x (Pair (Int 1, Pair (Pair (Int 2, Pair (Int 3, Int 4)), Int 5))))
let find_tree_y = callfresh (fun y -> find_tree' (Int 1) y)
let find_tree_xy = callfresh (fun x -> callfresh (fun y -> find_tree' x y))
let find_tree_complex = callfresh (fun x -> callfresh (fun a -> callfresh (fun b -> find_tree' (Pair (x, a)) (Pair (Pair (Int 1, b), a)))))
let find_2trees = callfresh (fun x -> conj (
  find_tree' x (Pair (Int 1, Pair (Int 2, Pair (Int 3, Pair (Int 4, Int 5))))),
  find_tree' x (Pair (Int 1, Pair (Pair (Int 2, Pair (Int 4, Int 5)), Int 3)))
))
