module StringMap = Map.Make (String)

(* Parse Input *)

let create_matrix input =
  let rec aux () =
    try
      let line = input_line input in
      Array.of_list (List.init (String.length line) (String.get line)) :: aux ()
    with e -> []
  in
  Array.of_list (aux ())

(* Queue *)

type 'a queue = 'a list * 'a list

let enqueue element = function
  | [], [] -> ([ element ], [])
  | [], toreverse -> (List.rev (element :: toreverse), [])
  | toprocess, toreverse -> (toprocess, element :: toreverse)

let dequeue = function
  | [], toreverse -> (
      match List.rev toreverse with
      | [] -> raise Not_found
      | h :: t -> (h, (t, [])))
  | h :: t, toreverse -> (h, (t, toreverse))

let empty = function [], [] -> true | _ -> false

(* Utility Functions *)

let ch_at matrix = function y, x -> matrix.(y).(x)
let str_of_coord = function x, y -> string_of_int x ^ "," ^ string_of_int y

let is_in_matrix matrix = function
  | y, x ->
      y >= 0 && x >= 0 && y < Array.length matrix && x < Array.length matrix.(0)

let coord_equals cA cB =
  match (cA, cB) with (y, x), (y', x') -> y = y' && x = x'

let coords = function c, _ -> c
let steps = function _, s -> s

(* Solution *)

let can_reach matrix curr target =
  is_in_matrix matrix target
  &&
  let c  = ch_at matrix curr in
  let c' = int_of_char c in
  let t  = ch_at matrix target in
  let t' = int_of_char t in
  abs (c' - t') <= 1 || c' >= t' || c = 'S' || (c = 'z' && t = 'E')

let adjacents_coord = function
  | y, x ->
      let top   = (y - 1, x) in
      let bot   = (y + 1, x) in
      let left  = (y, x - 1) in
      let right = (y, x + 1) in
      (top, bot, left, right)

let rec remove_unreachable matrix curr = function
  | [] -> []
  | h :: t ->
      if can_reach matrix curr h then h :: remove_unreachable matrix curr t
      else remove_unreachable matrix curr t

let get_list_adjacents matrix = function
  | (y, x) as pos ->
      let top, bot, left, right = adjacents_coord pos in
      remove_unreachable matrix pos [ top; bot; left; right ]

let rec enqueue_not_visited queue visited steps = function
  | h :: t ->
      let str = str_of_coord h in
      if StringMap.find_opt str visited = None then
        let queue = enqueue (h, steps) queue in
        enqueue_not_visited queue (StringMap.add str true visited) steps t
      else enqueue_not_visited queue visited steps t
  | [] -> (queue, visited)

let enq_adj_notvisited matrix e queue visited =
  let adjacents = get_list_adjacents matrix (coords e) in
  enqueue_not_visited queue visited (steps e + 1) adjacents

exception Empty_Queue

let rec bfs matrix target queue visited =
  if not (empty queue) then
    let e, q = dequeue queue in
    if ch_at matrix (coords e) = target then steps e
    else
      let q, v = enq_adj_notvisited matrix e q visited in
      bfs matrix target q v
  else raise Empty_Queue

let () =
  let queue  = ([ ((20, 0), 0) ], []) in
  let target = 'E' in
  let map    = StringMap.add (str_of_coord (20, 0)) true StringMap.empty in
  let matrix = create_matrix (open_in "input.txt") in
  let result = bfs matrix target queue map in
  Printf.printf "Part 1 solution: %d.\n" result
