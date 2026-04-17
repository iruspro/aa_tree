let () =
  let open Aa_tree in
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
  print string_of_int tree
