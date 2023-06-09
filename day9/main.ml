module Map = Map.Make (String)
open String
open List

type direction = L | R | U | D

let file = "input.txt"
let ic = open_in file

let get_dir = function
  | 'R' -> R
  | 'U' -> U
  | 'D' -> D
  | 'L' -> L
  | _ -> failwith ""

let parse line =
  (get_dir (get line 0), int_of_string (nth (String.split_on_char ' ' line) 1))

let manhattan_dist = function (x, y), (x', y') -> abs (x' - x) + abs (y' - y)

let move_tail = function
  | ((x, y) as head), ((x', y') as tail) -> (
      match manhattan_dist (head, tail) with
      | 2 when x = x' || y = y' ->
          if x > x' then (x' + 1, y')
          else if x < x' then (x' - 1, y')
          else if y > y' then (x, y' + 1)
          else (x, y' - 1)
      | 3 ->
          let x' = if x > x' then x' + 1 else x' - 1 in
          let y' = if y > y' then y' + 1 else y' - 1 in
          (x', y')
      | _ -> tail)

let create_key = function x, y -> string_of_int x ^ ", " ^ string_of_int y
let map = ref Map.empty

let move dir dist (head, tail) =
  let rec aux i = function
    | ((x, y) as head), ((x', y') as tail) ->
        if i != 0 then (
          let head =
            match dir with
            | U -> (x, y - 1)
            | D -> (x, y + 1)
            | R -> (x + 1, y)
            | L -> (x - 1, y)
          in
          let tail = move_tail (head, tail) in
          map := Map.add (create_key tail) true !map;
          aux (i - 1) (head, tail))
        else (head, tail)
  in
  aux dist (head, tail)

let solve =
  let rec aux head tail =
    try
      let line = input_line ic in
      let dir, dist = parse line in
      let head, tail = move dir dist (head, tail) in
      aux head tail
    with e -> Map.cardinal !map
  in
  aux (0, 0) (0, 0)

let () =
  seek_in ic 0;
  Printf.printf "Part 1 solution: %d.\n" solve
