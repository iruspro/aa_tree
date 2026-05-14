open Aa_tree

let level_of = function Nil -> 0 | Cons { level; _ } -> level

let rec all p = function
  | Nil -> true
  | Cons { left; right; key; _ } -> p key && all p left && all p right

let rec check_invariants = function
  | Nil -> true
  | Cons { left; right; key; level } ->
      let ll = level_of left in
      let rl = level_of right in
      let rrl =
        match right with Cons { right = rr; _ } -> level_of rr | Nil -> 0
      in
      level >= 1
      && ll = level - 1
      && (rl = level || rl = level - 1)
      && rrl < level
      && all (fun k -> k < key) left
      && all (fun k -> k > key) right
      && check_invariants left
      && check_invariants right

let rec in_order = function
  | Nil -> []
  | Cons { left; right; key; _ } -> in_order left @ [ key ] @ in_order right

let shuffle xs =
  let arr = Array.of_list xs in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  Array.to_list arr

let () =
  let leaf k = Cons { left = Nil; right = Nil; key = k; level = 1 } in
  let n5_7 = Cons { left = Nil; right = leaf 7; key = 5; level = 1 } in
  let n2 = Cons { left = leaf 1; right = leaf 3; key = 2; level = 2 } in
  let n8 = Cons { left = n5_7; right = leaf 9; key = 8; level = 2 } in
  let n12 = Cons { left = leaf 11; right = leaf 13; key = 12; level = 2 } in
  let n10 = Cons { left = n8; right = n12; key = 10; level = 3 } in
  let tree = Cons { left = n2; right = n10; key = 4; level = 3 } in
  print_endline "Initial:";
  print string_of_int tree;
  let tree = insert 6 tree in
  print_endline "\nAfter insert 6:";
  print string_of_int tree;
  let tree = delete 1 tree in
  print_endline "\nAfter delete 1:";
  print string_of_int tree;
  print_endline "\nSearch:";
  List.iter
    (fun k -> Printf.printf "  search %2d = %b\n" k (search k tree))
    [ 1; 5; 6; 10; 99 ];

  print_endline "\nInvariants stress test:";
  Random.init 42;
  let n = 50 in
  let keys = List.init n (fun i -> i + 1) in
  let sorted = List.sort compare keys in
  let inserts = shuffle keys in
  let check label k t =
    if not (check_invariants t) then begin
      Printf.printf "  FAIL: invariants violated after %s %d\n" label k;
      print string_of_int t;
      exit 1
    end
  in
  let stress =
    List.fold_left
      (fun t k ->
        let t' = insert k t in
        check "insert" k t';
        t')
      empty inserts
  in
  Printf.printf "  inserted %d keys: invariants OK\n" n;
  if in_order stress = sorted then print_endline "  in-order = sorted: OK"
  else print_endline "  in-order MISMATCH: FAIL";
  let deletes = shuffle (List.filteri (fun i _ -> i mod 2 = 0) keys) in
  let stress =
    List.fold_left
      (fun t k ->
        let t' = delete k t in
        check "delete" k t';
        t')
      stress deletes
  in
  Printf.printf "  deleted %d keys: invariants OK\n" (List.length deletes);
  let expected = List.filter (fun k -> not (List.mem k deletes)) sorted in
  if in_order stress = expected then
    print_endline "  remaining keys match expected: OK"
  else print_endline "  remaining MISMATCH: FAIL"
