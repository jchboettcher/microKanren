open Ast

let rec walk (u : exp) ((s, n) : state) : exp =
  begin match u with
    | Var x ->
      begin match (List.assoc_opt x s) with
        | Some u' -> walk u' (s, n)
        | None -> u
      end
    | _ -> u
  end

let ext_s (x : var) (u : exp) ((s, n) : state) : state =
  ((x, u) :: s), n

let rec unify (u : exp) (v : exp) (s : state) : state option =
  let u' = walk u s in
  let v' = walk v s in
  if u' = v' then Some s else
  begin match u', v' with
    | Var x, _ -> Some (ext_s x v' s)
    | _, Var y -> Some (ext_s y u' s)
    | Pair (u1, u2), Pair (v1, v2) ->
      begin match unify u1 v1 s with
        | Some s1 -> unify u2 v2 s1
        | None -> None
      end
    | _ -> None
  end

let fresh (n : int) : exp =
  Var ("x" ^ (string_of_int n))

let rec mplus (str1 : state stream) (str2 : state stream) : state stream =
  begin match str1 with
    | MZero -> str2
    | Fun f -> Fun (fun () -> mplus str2 (f ()))
    | Mat (s, str) -> Mat (s, mplus str str2)
  end

let rec bind (str : state stream) (g : goal) : state stream =
  begin match str with
    | MZero -> MZero
    | Fun f -> Fun (fun () -> bind (f ()) g)
    | Mat (s, str') -> mplus (g s) (bind str' g)
  end

let equal ((u, v) : exp * exp) : goal = fun s ->
  begin match unify u v s with
    | Some s' -> Mat (s', MZero)
    | None -> MZero
  end

let callfresh (f : exp -> goal) : goal = fun (s, n) ->
  f (fresh n) (s, n+1)

let disj ((g1, g2) : goal * goal) : goal = fun s ->
  mplus (g1 s) (g2 s)

let conj ((g1, g2) : goal * goal) : goal = fun s ->
  bind (g1 s) g2
