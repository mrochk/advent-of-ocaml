let file = "input.txt"
let ic = open_in file

let parse_crates line =
  [
    String.get line 1;
    String.get line 5;
    String.get line 9;
    String.get line 13;
    String.get line 17;
    String.get line 21;
    String.get line 25;
    String.get line 29;
    String.get line 33;
  ]

let rec add_crates = function
  | h :: t, h' :: t' ->
      let tails = (t, t') in
      if h' = ' ' then h :: add_crates tails else (h' :: h) :: add_crates tails
  | _, _ -> []

let empty_slist =
  let rec aux n = if n < 0 then [] else [] :: aux (n - 1) in
  aux 8

let fill_slist =
  let rec aux c res =
    if c = 0 then res
    else
      let line = input_line ic in
      let elements = parse_crates line in
      aux (c - 1) (add_crates (res, elements))
  in
  aux 8 empty_slist

let init_slist =
  let rec aux = function h :: t -> List.rev h :: aux t | [] -> [] in
  aux fill_slist

let parse_instr line =
  let l = String.split_on_char ' ' line in
  ( int_of_string (List.nth l 1),
    int_of_string (List.nth l 3),
    int_of_string (List.nth l 5) )

let rec get_slist n = function
  | h :: t as l ->
      if n = 0 then ([], l)
      else
        let x = get_slist (n - 1) t in
        (h :: fst x, snd x)
  | [] -> ([], [])

let rec replace_stacklist l index = function
  | h :: t ->
      if index = 0 then l :: t else h :: replace_stacklist l (index - 1) t
  | [] -> failwith ""

let rec add_stacklist l index = function
  | h :: t ->
      if index = 0 then (l @ h) :: t else h :: add_stacklist l (index - 1) t
  | [] -> failwith ""

let moveA slist howmuch from where =
  let l, rest = get_slist howmuch (List.nth slist from) in
  let res = replace_stacklist rest from slist in
  add_stacklist (List.rev l) where res

let moveB slist howmuch from where =
  let l, rest = get_slist howmuch (List.nth slist from) in
  let res = replace_stacklist rest from slist in
  add_stacklist l where res

let rec slist_to_str = function
  | h :: t -> String.make 1 (List.hd h) ^ slist_to_str t
  | [] -> ""

let solve =
  let rec solve' stlA stlB =
    try
      let line = input_line ic in
      let howmuch, from, where = parse_instr line in
      let stlA = moveA stlA howmuch (from - 1) (where - 1) in
      let stlB = moveB stlB howmuch (from - 1) (where - 1) in
      solve' stlA stlB
    with e -> (slist_to_str stlA, slist_to_str stlB)
  in
  seek_in ic 0;
  let a, b = (init_slist, init_slist) in
  seek_in ic 325;
  solve' a b

let () =
  let p1, p2 = solve in
  Printf.printf "Part 1 solution: %s.\n" p1;
  Printf.printf "Part 2 solution: %s.\n" p2