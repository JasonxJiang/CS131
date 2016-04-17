type tree = Leaf1 | Node1 of int * tree * tree

let rec in_order = function
  | Leaf1 -> []
  | Node1(i,l,r) -> in_order l @ (i :: in_order r);;

let rec preorder = function
  | Leaf1 -> []
  | Node1(x, l, r) -> x::preorder l @ preorder r

  let rec postorder = function
  | Leaf1 -> []
  | Node1(x, l, r) -> postorder l @ postorder r @ [x]

let tree1 = Node1(5, Node1(3,Node1(1,Leaf1,Leaf1),Leaf1) ,Node1(8,Node1(6,Leaf1,Node1(7,Leaf1,Leaf1)),Leaf1));;

  (* save this for later working but reversed *)
(*List.fold_left (fun row firstPartMatrix -> List.map2 (fun x y-> x::y) firstPartMatrix row) [[];[];[]] m;;
*)