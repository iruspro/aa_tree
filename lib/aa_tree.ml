type 'a t = Nil | Cons of { left : 'a t; right : 'a t; key : 'a; level : int }

(* GETTERS *)
let empty = Nil

(* HELPERS *)
let rotate_right = function
  | Nil -> Nil
  | Cons { left = Nil; _ } as tree -> tree
  | Cons ({ left = Cons l_node; _ } as tree) ->
      Cons { l_node with right = Cons { tree with left = l_node.right } }

let rotate_left = function
  | Nil -> Nil
  | Cons { right = Nil; _ } as tree -> tree
  | Cons ({ right = Cons r_node; _ } as tree) ->
      Cons { r_node with left = Cons { tree with right = r_node.left } }

let skew = function
  | Nil -> Nil
  | Cons { left = Nil; _ } as tree -> tree
  | Cons { left = Cons { level = l_level; _ }; level; _ } as tree ->
      if l_level = level then rotate_right tree else tree

let split = function
  | Nil -> Nil
  | Cons { right = Nil; _ } as tree -> tree
  | Cons { right = Cons { right = Nil; _ }; _ } as tree -> tree
  | Cons { right = Cons { right = Cons { level = rr_level; _ }; _ }; level; _ }
    as tree -> (
      if rr_level <> level then tree
      else
        match rotate_left tree with
        | Cons n -> Cons { n with level = n.level + 1 }
        | Nil -> assert false)
