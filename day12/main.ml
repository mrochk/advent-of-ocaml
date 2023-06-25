module StrSet = Set.Make (String)

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

exception Empty_Queue

let enq element = function
  | [], [] -> ([ element ], [])
  | [], toreverse -> (List.rev (element :: toreverse), [])
  | toprocess, toreverse -> (toprocess, element :: toreverse)

let deq = function
  | [], [] -> raise Empty_Queue
  | [], h :: t -> (h, (t, []))
  | h :: t, toreverse -> (h, (t, toreverse))

let empty = function [], [] -> true | _ -> false

(* Utility Functions *)

let string_of_coord = function x, y -> string_of_int x ^ "," ^ string_of_int y
let exists set e = not (StrSet.find_opt e set = None)
let ch_at matrix = function y, x -> matrix.(y).(x)
let coords = function c, _ -> c
let dist = function _, s -> s

let coord_equals cA cB =
  match (cA, cB) with (y, x), (y', x') -> y = y' && x = x'

let in_matrix matrix = function
  | y, x ->
      y >= 0 && x >= 0 && y < Array.length matrix && x < Array.length matrix.(0)

let adj_list = function
  | y, x -> [ (y - 1, x); (y + 1, x); (y, x - 1); (y, x + 1) ]

let can_reach matrix curr target =
  in_matrix matrix target
  &&
  let c = ch_at matrix curr in
  let c' = int_of_char c in
  let t = ch_at matrix target in
  let t' = int_of_char t in
  abs (c' - t') <= 1 || c' >= t' || c = 'S' || (c = 'z' && t = 'E')

let rec enq_reachbl matrix e queue visited = function
  | h :: t ->
      let str = string_of_coord h in
      if can_reach matrix (coords e) h && not (exists visited str) then
        let queue = enq (h, dist e + 1) queue in
        let visited = StrSet.add str visited in
        enq_reachbl matrix e queue visited t
      else enq_reachbl matrix e queue visited t
  | [] -> (queue, visited)

let get_a's matrix =
  let rec aux r =
    if r < Array.length matrix then
      let rec aux' c =
        if c = Array.length matrix.(0) then []
        else if ch_at matrix (r, c) = 'a' then (r, c) :: aux' (c + 1)
        else aux' (c + 1)
      in
      aux' 0 @ aux (r + 1)
    else []
  in
  aux 0

(* Solution *)

let rec bfs matrix queue visited curr_min =
  if not (empty queue) then
    let e, q = deq queue in
    if dist e >= curr_min then 
      curr_min else
    match ch_at matrix (coords e) with
    | 'E' -> (* found shortest path to E *) dist e
    | _ ->
        let q, v = enq_reachbl matrix e q visited (adj_list (coords e)) in
        bfs matrix q v curr_min
  else (* can't reach E *) curr_min

let rec solve matrix curr_min = function
  | [] -> curr_min
  | h :: t ->
      let queue = ([ (h, 0) ], []) in
      let map = StrSet.add (string_of_coord h) StrSet.empty in
      let m = (bfs matrix queue map curr_min) 
      in solve matrix m t

let () =
  let matrix = create_matrix (open_in "input.txt") in
  let part1 = solve matrix Int.max_int [ (20, 0) ] in
  let part2 = solve matrix Int.max_int (get_a's matrix) in
  Printf.printf "Part 1 solution: %d.\n" part1;
  Printf.printf "Part 2 solution: %d.\n" part2
