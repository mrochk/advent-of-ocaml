open String
open Char
open Printf
module Map = Map.Make (Char)

let file = "input.txt"
let ic = open_in file
let map = ref Map.empty;;

for i = 0 to 25 do
  map := Map.add (unsafe_chr (code 'a' + i)) (i + 1) !map
done
;;

for i = 26 to 51 do
  map := Map.add (unsafe_chr (code 'A' + (i - 26))) (i + 1) !map
done

let dup str =
  let len_half = length str / 2 in
  let rec dup' a b =
    if a = len_half then dup' 0 (b + 1)
    else
      let c = str.[a] in
      if c = str.[b] then c else dup' (a + 1) b
  in
  dup' 0 len_half

let solve =
  let rec solve sum =
    try
      let line = input_line ic in
      let prio = Map.find (dup line) !map in
      solve (sum + prio)
    with e -> sum
  in
  solve 0

let () = printf "Part 1 solution: %d.\n" solve
