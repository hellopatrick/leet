(* Given the root of a binary search tree, and a target K, return two nodes in the tree whose sum equals K. *)

module IS = Set.Make (Int)

let solve t k =
  let rec aux t seen =
    match t with
    | Rbtree.Empty ->
        Either.left seen
    | Rbtree.Node (_, x, l, r) -> (
        let dv = k - x in
        if IS.mem dv seen then
          if x <= dv then Either.right (x, dv) else Either.right (dv, x)
        else
          let seen = IS.add x seen in
          match aux l seen with Either.Left ls -> aux r ls | res -> res )
  in
  match aux t IS.empty with Either.Right res -> Some res | _ -> None

let%test "20" =
  let t = Rbtree.of_list [5; 10; 11; 15; 15] in
  match solve t 20 with Some (5, 15) -> true | _ -> false

let%test "12" =
  let t = Rbtree.of_list [5; 3; 6; 2; 4; 7] in
  match solve t 12 with Some (5, 7) -> true | _ -> false

let%test "28" =
  let t = Rbtree.of_list [5; 3; 6; 2; 4; 7] in
  Option.is_none (solve t 28)
