let input = open_in "input.txt"

type operation = Multiply | Sum
type operand = Old | New of int

type monkey = {
  mutable count : int;
  mutable items : int list;
  operation : operation;
  operand : operand;
  test : int;
  ttrue : int;
  ffalse : int;
}

let parse_items line =
  let line = String.trim line in
  let line = String.sub line 16 (String.length line - 16) in
  let list = String.split_on_char ',' line in
  let list = List.map String.trim list in
  List.map int_of_string list

let parse_operation line =
  let line = String.trim line in
  let line = String.sub line 21 (String.length line - 21) in
  match String.split_on_char ' ' line with
  | [ "+"; "old" ] -> (Sum, Old)
  | [ "*"; "old" ] -> (Multiply, Old)
  | [ "+"; n ] -> (Sum, New (int_of_string n))
  | [ "*"; n ] -> (Multiply, New (int_of_string n))
  | _ -> failwith ""

let parse_test line =
  let line = String.trim line in
  let list = String.split_on_char ' ' line in
  int_of_string (List.nth list 3)

let parse_if line =
  let line = String.trim line in
  let list = String.split_on_char ' ' line in
  int_of_string (List.nth list 5)

let rec make_list n =
  if n = 0 then []
  else
    let _ = input_line input in
    let line = input_line input in
    let items = parse_items line in
    let line = input_line input in
    let operation, operand = parse_operation line in
    let line = input_line input in
    let test = parse_test line in
    let line = input_line input in
    let ttrue = parse_if line in
    let line = input_line input in
    let ffalse = parse_if line in
    let _ = if n = 1 then "" else input_line input in
    { count = 0; items; operation; operand; test; ttrue; ffalse }
    :: make_list (n - 1)

let monkeys = Array.of_list (make_list 8)

let compute_wlevel item = function
  | Multiply, Old -> item * item
  | Sum, Old -> item + item
  | Multiply, New n -> item * n
  | Sum, New n -> item + n

let rec remove element = function
  | [] -> []
  | h :: t -> if h = element then t else h :: remove element t

let rec step m =
  let rec aux = function
    | [] -> ()
    | h :: t ->
        let n = compute_wlevel h (m.operation, m.operand) / 3 in
        let () =
          if n mod m.test = 0 then
            monkeys.(m.ttrue).items <- n :: monkeys.(m.ttrue).items
          else monkeys.(m.ffalse).items <- n :: monkeys.(m.ffalse).items
        in
        aux t
  in
  let () = aux m.items in
  m.items <- []

let round n =
  let rec aux i =
    if i = n then ()
    else
      let () =
        monkeys.(i).count <- monkeys.(i).count + List.length monkeys.(i).items
      in
      let () = step monkeys.(i) in
      aux (i + 1)
  in
  aux 0

let () =
  for i = 1 to 20 do
    round 8
  done

let rec insert element = function
  | h :: t as l -> if element > h then element :: l else h :: insert element t
  | [] -> [ element ]

let rec cut n = function
  | h :: t -> if n = 0 then [] else h :: cut (n - 1) t
  | [] -> []

let compute_res =
  let rec aux i res =
    if i = Array.length monkeys then res
    else aux (i + 1) (cut 2 (insert monkeys.(i).count res))
  in
  aux 0 []

let result = match compute_res with [ a; b ] -> a * b | _ -> failwith ""
let () = Printf.printf "Part 1 solution: %d.\n" result
