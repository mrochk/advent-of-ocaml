let file = "input.txt"
let ic = open_in file

let cut n l =
  let rec aux n = function
    | h :: t -> if n = 0 then [] else h :: aux (n - 1) t
    | [] -> []
  in
  aux n l

let rec is_in c = function
  | h :: t -> if h = c then true else is_in c t
  | [] -> false

let rec duplicates = function
  | h :: t -> if is_in h t then true else duplicates t
  | [] -> false

let solve1 =
  let rec aux last4 count =
    if List.length last4 >= 4 && not (duplicates last4) then count
    else
      let ch = input_char ic in
      aux (cut 4 (ch :: last4)) (count + 1)
  in
  seek_in ic 0;
  aux [] 0

let solve2 =
  let rec aux last14 count =
    if List.length last14 >= 14 && not (duplicates last14) then count
    else
      let ch = input_char ic in
      aux (cut 14 (ch :: last14)) (count + 1)
  in
  seek_in ic 0;
  aux [] 0

let () =
  Printf.printf "Part 1 solution: %d.\n" solve1;
  Printf.printf "Part 2 solution: %d.\n" solve2
