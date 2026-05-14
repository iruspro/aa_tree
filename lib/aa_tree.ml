type 'a t = Nil | Cons of { left : 'a t; right : 'a t; key : 'a; level : int }

(* GETTERS *)
let empty = Nil

(* HELPERS *)
let update_left f = function
  | Nil -> Nil
  | Cons ({ left; _ } as node) -> Cons { node with left = f left }

let update_right f = function
  | Nil -> Nil
  | Cons ({ right; _ } as node) -> Cons { node with right = f right }

let left = function Nil -> Nil | Cons { left; _ } -> left
let right = function Nil -> Nil | Cons { right; _ } -> right
let level_of = function Nil -> 0 | Cons { level; _ } -> level

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
  | Cons { key = k; _ } as tree ->
      if key = k then tree
      else
        let tree' =
          if key < k then update_left (insert key) tree
          else update_right (insert key) tree
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

let successor tree =
  let rec leftmost t = match left t with Nil -> t | _ -> leftmost (left t) in
  leftmost (right tree)

let predecessor tree =
  let rec rightmost t =
    match right t with Nil -> t | _ -> rightmost (right t)
  in
  rightmost (left tree)

let decrease_level = function
  | Nil -> Nil
  | Cons ({ left; right; level; _ } as node) as tree ->
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

let rec delete key = function
  | Nil -> Nil
  | Cons ({ key = k; right; _ } as node) as tree ->
      let tree' =
        if key < k then update_left (delete key) tree
        else if key > k then update_right (delete key) tree
        else
          match successor tree with
          | Nil -> Nil
          | Cons { key = s_key; _ } ->
              Cons { node with key = s_key; right = delete s_key right }
      in
      tree' |> decrease_level |> skew |> update_right skew
      |> update_right (update_right skew)
      |> split |> update_right split

let to_string to_s tree =
  let rec build rank depth = function
    | Nil -> (rank, None, [])
    | Cons { key; left; right; level; _ } ->
        let rank, l_root, l_nodes = build rank (depth + 1) left in
        let my_rank = rank in
        let right_depth =
          match right with
          | Cons { level = rl; _ } when rl = level -> depth
          | _ -> depth + 1
        in
        let rank, r_root, r_nodes = build (rank + 1) right_depth right in
        let info = (my_rank, depth, level, to_s key, l_root, r_root) in
        (rank, Some my_rank, l_nodes @ [ info ] @ r_nodes)
  in
  let n, _, nodes = build 0 0 tree in
  if n = 0 then ""
  else
    let key_w =
      List.fold_left
        (fun a (_, _, _, s, _, _) -> max a (String.length s))
        1 nodes
    in
    let col_unit = key_w + 1 in
    let canvas_w = ((n - 1) * col_unit) + key_w in
    let max_depth =
      List.fold_left (fun a (_, d, _, _, _, _) -> max a d) 0 nodes
    in
    let canvas_h = (2 * max_depth) + 1 in
    let canvas = Array.init canvas_h (fun _ -> Bytes.make canvas_w ' ') in
    let depths = Array.make n 0 in
    List.iter (fun (r, d, _, _, _, _) -> depths.(r) <- d) nodes;
    List.iter
      (fun (r, d, _, s, _, _) ->
        Bytes.blit_string s 0 canvas.(d * 2) (r * col_unit) (String.length s))
      nodes;
    List.iter
      (fun (r, d, _, s, l_root, r_root) ->
        let my_col = r * col_unit in
        let s_len = String.length s in
        (match l_root with
        | Some lr ->
            let mid = (my_col + (lr * col_unit)) / 2 in
            Bytes.set canvas.((d * 2) + 1) mid '/'
        | None -> ());
        match r_root with
        | None -> ()
        | Some rr ->
            let rc = rr * col_unit in
            if depths.(rr) = d then
              for c = my_col + s_len to rc - 1 do
                Bytes.set canvas.(d * 2) c '-'
              done
            else
              let mid = (my_col + rc) / 2 in
              Bytes.set canvas.((d * 2) + 1) mid '\\')
      nodes;
    let row_level = Array.make canvas_h None in
    List.iter
      (fun (_, d, lv, _, _, _) ->
        if row_level.(d * 2) = None then row_level.(d * 2) <- Some lv)
      nodes;
    let buf = Buffer.create 256 in
    for r = 0 to canvas_h - 1 do
      (match row_level.(r) with
      | Some lv -> Buffer.add_string buf (Printf.sprintf "%2d | " lv)
      | None -> Buffer.add_string buf "   | ");
      Buffer.add_string buf (Bytes.to_string canvas.(r));
      Buffer.add_char buf '\n'
    done;
    Buffer.contents buf

let print to_s tree = print_string (to_string to_s tree)
