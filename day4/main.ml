let file = "input.txt"
let ic = open_in file

let parse line =
  let split sep str = String.split_on_char sep str in
  let ios s = int_of_string s in
  match split ',' line with
  | [ a; b ] -> (
      match split '-' a @ split '-' b with
      | [ a; b; c; d ] -> ((ios a, ios b), (ios c, ios d))
      | _ -> failwith "")
  | _ -> failwith ""

let contained = function
  | (a, b), (c, d) -> (a <= c && b >= d) || (c <= a && d >= b)

let overlap = function
  | (a, b), (c, d) ->
      (a <= c && b >= c)
      || (a <= d && b >= d)
      || (c <= a && d >= a)
      || (c <= b && d >= b)

let solve =
  let rec solve' over cont =
    try
      let line = input_line ic in
      let nums = parse line in
      if overlap nums then
        if contained nums then solve' (over+1) (cont+1)
        else solve' (over+1) cont
      else solve' over cont
    with e -> (cont, over)
  in
  solve' 0 0

let p1, p2 = solve
let () = Printf.printf "Part 1 solution: %d.\n" p1
let () = Printf.printf "Part 2 solution: %d.\n" p2
