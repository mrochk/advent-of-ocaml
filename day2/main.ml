open Char
open Printf
module Map = Map.Make (Char)

let file = "input.txt"
let ic = open_in file
let map = ref Map.empty;;

for i = 1 to 3 do
  map := Map.add (unsafe_chr (code 'W' + i)) i !map
done

let get_score = function
  | 'A', 'X' | 'B', 'Y' | 'C', 'Z' -> 3
  | 'A', 'Y' | 'B', 'Z' | 'C', 'X' -> 6
  | _ -> 0

let rec solve =
  let rec solve' score =
    try
      let line = input_line ic in
      let played = (line.[0], line.[2]) in
      solve' (score + get_score played + Map.find (snd played) !map)
    with e -> score
  in
  solve' 0

let () = printf "Part 1 solution: %d\n" solve
