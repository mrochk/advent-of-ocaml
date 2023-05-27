let input = open_in "input.txt"

let parse line =
  let split = String.split_on_char ' ' line in
  let op = List.hd split in
  if op = "addx" then int_of_string (List.nth split 1) else 0

let check_cycle = function
  | 20 | 60 | 100 | 140 | 180 | 220 -> true
  | _ -> false

let rec execute steps cycle regX =
  if steps = 1 then if check_cycle cycle then regX * cycle else 0
  else if check_cycle cycle then
    (regX * cycle) + execute (steps - 1) (cycle + 1) regX
  else execute (steps - 1) (cycle + 1) regX

let solve =
  seek_in input 0;
  let rec aux cycle sum regX =
    try
      let line = input_line input in
      let add = parse line in
      let steps = if add = 0 then 1 else 2 in
      let sum = sum + execute steps cycle regX in
      aux (cycle + steps) sum (regX + add)
    with e -> sum
  in
  aux 1 0 1

let () = Printf.printf "Part 1 solution: %d.\n" solve
