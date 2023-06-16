open Printf
open List

let file = "input.txt"
let ic = open_in file
let empty = ""

let cut l =
  let rec cut' c = function
    | [] -> []
    | h :: t -> if c - 1 = 0 then [ h ] else h :: cut' (c - 1) t
  in
  cut' 3 l

let add n l =
  let rec add' n = function
    | [] -> [ n ]
    | h :: t -> if n > h then n :: h :: t else h :: add' n t
  in
  cut (add' n l)

let solve =
  let rec solve' sum res =
    try
      let line = input_line ic in
      if line = empty then solve' 0 (add sum res)
      else
        let n = int_of_string line in
        solve' (sum + n) res
    with e -> res
  in
  solve' 0 []

let rec sum = function [] -> 0 | h :: t -> h + sum t

let () =
  let result = solve in
  let () = printf "Part 1 solution : %d.\n" (hd result) in
  let () = printf "Part 2 solution : %d.\n" (sum result) in
  flush stdout
