type color = Red | Black

type t = Empty | Node of color * int * t * t

let rec mem x = function
  | Empty ->
      false
  | Node (_, y, l, r) ->
      if x < y then mem x l else if x > y then mem x r else true

let balance = function
  | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | a, b, c, d ->
      Node (a, b, c, d)

let insert s x =
  let rec ins = function
    | Empty ->
        Node (Red, x, Empty, Empty)
    | Node (color, y, a, b) ->
        if x <= y then balance (color, y, ins a, b)
        else balance (color, y, a, ins b)
  in
  match ins s with
  | Node (_, y, a, b) ->
      Node (Black, y, a, b)
  | Empty ->
      (* guaranteed to be nonempty *)
      failwith "RBT insert failed with ins returning leaf"

let of_list = List.fold_left insert Empty
