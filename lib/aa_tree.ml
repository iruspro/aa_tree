type 'a t = Nil | Cons of { left : 'a t; right : 'a t; key : 'a; level : int }

(* GETTERS *)
let empty = Nil

(* HELPERS *)
let rotate_right = function
  | Nil -> Nil
  | Cons { left = Nil; _ } as tree -> tree
  | Cons ({ left = Cons l_node; _ } as node) ->
      Cons { l_node with right = Cons { node with left = l_node.right } }

let rotate_left = function
  | Nil -> Nil
  | Cons { right = Nil; _ } as tree -> tree
  | Cons ({ right = Cons r_node; _ } as node) ->
      Cons { r_node with left = Cons { node with right = r_node.left } }

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

(* FUNCTIONS *)
let rec search key = function
  | Nil -> false
  | Cons { key = k; left; right; _ } ->
      if key = k then true
      else if key < k then search key left
      else search key right

let rec insert key = function
  | Nil -> Cons { left = Nil; right = Nil; key; level = 1 }
  | Cons ({ key = k; left; right; _ } as node) as tree ->
      if key = k then tree
      else
        let tree' =
          if key < k then Cons { node with left = insert key left }
          else Cons { node with right = insert key right }
        in
        tree' |> skew |> split

let rec t_min = function
  | Nil -> None
  | Cons { left = Nil; key; _ } -> Some key
  | Cons { left; _ } -> t_min left

let rec t_max = function
  | Nil -> None
  | Cons { right = Nil; key; _ } -> Some key
  | Cons { right; _ } -> t_max right

let successor = function
  | Nil -> Nil
  | Cons { right; _ } ->
      let rec leftmost = function
        | Nil -> Nil
        | Cons { left = Nil; _ } as node -> node
        | Cons { left; _ } -> leftmost left
      in
      leftmost right

let predecessor = function
  | Nil -> Nil
  | Cons { left; _ } ->
      let rec rightmost = function
        | Nil -> Nil
        | Cons { right = Nil; _ } as node -> node
        | Cons { right; _ } -> rightmost right
      in
      rightmost left

let decrease_level = function
  | Nil -> Nil
  | Cons ({ left; right; level; _ } as node) as tree ->
      let level_of = function Nil -> 0 | Cons { level; _ } -> level in
      let should_be = min (level_of left) (level_of right) + 1 in
      if should_be >= level then tree
      else
        let right' =
          match right with
          | Cons ({ level = r_level; _ } as r_node) when should_be < r_level ->
              Cons { r_node with level = should_be }
          | _ -> right
        in
        Cons { node with level = should_be; right = right' }

let update_right f = function
  | Nil -> Nil
  | Cons ({ right; _ } as node) -> Cons { node with right = f right }

let rec delete key = function
  | Nil -> Nil
  | Cons ({ key = k; left; right; _ } as node) as tree ->
      let tree' =
        if key < k then Cons { node with left = delete key left }
        else if key > k then Cons { node with right = delete key right }
        else
          match successor tree with
          | Nil -> Nil
          | Cons { key = s_key; _ } ->
              Cons { node with key = s_key; right = delete s_key right }
      in
      tree' |> decrease_level |> skew |> update_right skew
      |> update_right (update_right skew)
      |> split |> update_right split
