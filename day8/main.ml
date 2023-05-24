let file = "input.txt"
let ic = open_in file

let parse line =
  let rec aux i =
    if i = String.length line then []
    else int_of_string (String.sub line i 1) :: aux (i + 1)
  in
  aux 0

let create_forest =
  let rec aux matrix =
    try
      let line = input_line ic in
      let row = parse line in
      aux (row :: matrix)
    with e -> matrix
  in
  aux [] |> List.rev

let rec check n = function h :: t -> h < n && check n t | [] -> true

let split_lr col l =
  let rec aux i l r = function
    | [] -> (l |> List.rev, r |> List.rev)
    | h :: t ->
        if i > 0 then aux (i - 1) (h :: l) r t
        else if i < 0 then aux (i - 1) l (h :: r) t
        else aux (i - 1) l r t
  in
  aux col [] [] l

let rec split_tb col row matrix =
  let rec aux i top bot = function
    | [] -> (top |> List.rev, bot |> List.rev)
    | h :: t ->
        if i < row then aux (i + 1) (List.nth h col :: top) bot t
        else if i > row then aux (i + 1) top (List.nth h col :: bot) t
        else aux (i + 1) top bot t
  in
  aux 0 [] [] matrix

let visible_line col n l =
  let left, right = split_lr col l in
  col = 0 || col = List.length l || check n left || check n right

let visible_row col row n matrix =
  let top, bot = split_tb row col matrix in
  row = 0 || row = List.length matrix || check n top || check n bot

let visible_trees row line matrix =
  let rec aux col =
    if col < List.length line - 1 then
      let n = List.nth line col in
      if visible_line col n line || visible_row row col n matrix then
        1 + aux (col + 1)
      else aux (col + 1)
    else 0
  in
  2 + aux 1

let solve matrix =
  let rec aux row = function
    | h :: t -> visible_trees row h matrix + aux (row + 1) t
    | [] -> 0
  in
  aux 0 matrix

let () =
  seek_in ic 0;
  let forest = create_forest in
  Printf.printf "Part 1 solution: %d.\n" (solve forest)
