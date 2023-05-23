open String
open List
open Int

type 'a filetree = Dir of string * int * 'a filetree list | File of int
type cmd = CD | LS | DIR | FILE

let file = "input.txt"
let ic = open_in file
let move_out = ".."
let space_total = 70000000
let space_required = 30000000

let parse_cmd line =
  let l = split_on_char ' ' line in
  if hd l = "$" then if nth l 1 = "cd" then CD else LS
  else if hd l = "dir" then DIR
  else FILE

let parse_file line = int_of_string (hd (split_on_char ' ' line))
let parse_dir line = nth (split_on_char ' ' line) 1
let parse_cd line = nth (split_on_char ' ' line) 2

let find_dir name tree =
  let rec find_dir' = function
    | (Dir (n, _, _) as h) :: t -> if n = name then h else find_dir' t
    | [] -> failwith ""
    | _ :: t -> find_dir' t
  in
  match tree with Dir (_, _, l) -> find_dir' l | _ -> failwith ""

let append_dir name = function
  | Dir (n, size, sons) -> Dir (n, size, Dir (name, 0, []) :: sons)
  | _ -> failwith ""

let append_file size = function
  | Dir (name, s, sons) -> Dir (name, s, File size :: sons)
  | _ -> failwith ""

let rec remove_dir name = function
  | (Dir (n, _, _) as h) :: t -> if n = name then t else h :: remove_dir name t
  | h :: t -> h :: remove_dir name t
  | [] -> []

let rec create_filetree tree =
  try
    let line = input_line ic in
    match tree with
    | Dir (name, size, l) -> (
        match parse_cmd line with
        | DIR -> create_filetree (append_dir (parse_dir line) tree)
        | FILE -> create_filetree (append_file (parse_file line) tree)
        | LS -> create_filetree tree
        | CD ->
            let dirname = parse_cd line in
            if dirname = move_out then tree
            else
              let dir = find_dir dirname tree in
              let l = remove_dir dirname l in
              create_filetree (Dir (name, size, create_filetree dir :: l)))
    | _ -> exit 1
  with e -> tree

let rec compute_size tree =
  let rec aux = function [] -> 0 | h :: t -> compute_size h + aux t in
  match tree with Dir (n, _, l) -> aux l | File size -> size

let rec replace_size tree =
  let rec aux = function
    | [] -> []
    | h :: t -> (
        match h with Dir (_, _, _) -> replace_size h :: aux t | _ -> aux t)
  in
  match tree with
  | Dir (name, size, l) -> Dir (name, compute_size tree, aux l)
  | File _ as f -> f

let get_space_to_free = function
  | Dir (_, size, _) -> space_required - (space_total - size)
  | _ -> failwith ""

let ftree = replace_size (create_filetree (Dir ("/", 0, [])))
let to_free = get_space_to_free ftree

let solve filetree =
  let rec part1 ft =
    let rec aux = function h :: t -> part1 h + aux t | [] -> 0 in
    match ft with
    | Dir (_, size, l) -> if size <= 100000 then size + aux l else aux l
    | File _ -> 0
  in
  let rec part2 ft =
    let rec aux = function [] -> max_int | h :: t -> min (part2 h) (aux t) in
    match ft with
    | Dir (_, size, sons) ->
        if size >= to_free then min size (aux sons) else aux sons
    | File _ -> failwith ""
  in
  (part1 filetree, part2 filetree)

let () =
  seek_in ic 0;
  let p1, p2 = solve ftree in
  Printf.printf "Part 1 solution: %d.\n" p1;
  Printf.printf "Part 2 solution: %d.\n" p2
