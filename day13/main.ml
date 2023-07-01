open String

type t = E of int | S of t list

let to_int c = int_of_char c - int_of_char '0'

let rec create_list line =
  let rec aux i =
    if i < length line then
      match line.[i] with
      | ']' -> ([], i)
      | ',' -> aux (i + 1)
      | '[' ->
          let l, i = aux (i + 1) in
          let rest, i = aux (i + 1) in
          (S l :: rest, i)
      | c ->
          if line.[i + 1] = '0' then
            let l, i = aux (i + 2) in
            (E 10 :: l, i)
          else
            let l, i = aux (i + 1) in
            (E (to_int c) :: l, i)
    else ([], i)
  in
  match aux 0 with S l :: _, _ -> l | _ -> exit 1

let rec compare_sublists = function
  | S [] :: t, [] -> false
  | S [ S [] ] :: t, [] -> false
  | E e :: t, E e' :: t' -> if e > e' then false else compare_sublists (t, t')
  | S s :: t, S s' :: t' -> compare_sublists (s, s') && compare_sublists (t, t')
  | E e :: t, S s :: t' -> compare_sublists ([ E e ], s) && compare_sublists (t, t')
  | S s :: t, E e :: t' -> compare_sublists (s, [ E e ]) && compare_sublists (t, t')
  | _ -> true

let rec compare_lists = function
  | E e :: t, E e' :: t' -> if e > e' then false else compare_lists (t, t')
  | S s :: t, S s' :: t' -> compare_sublists (s, s') && compare_lists (t', t')
  | S s :: t, E e :: t' -> compare_sublists (s, [ E e ]) && compare_lists (t', t')
  | E e :: t, S s :: t' -> compare_sublists ([ E e ], s) && compare_lists (t, t')
  | a, [] when a != [] -> false
  | _ -> true

let rec solve input =
  let rec aux index sum =
    try
      let line = input_line input in
      if line = "" then aux (index + 1) sum
      else
        let line' = input_line input in
        if compare_lists (create_list line, create_list line') then
          aux index (sum + index)
        else aux index sum
    with e -> sum
  in
  aux 1 0

let () =
  let test = open_in "test.txt" in
  let input = open_in "input.txt" in
  Printf.printf "Test input solution = %d.\n" (solve test);
  Printf.printf "Part 1 solution (wrong) = %d.\n" (solve input)
