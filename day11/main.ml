let input = open_in "input.txt"

type operation = Mul | Sum
type operand   = Old | N of int

type monkey = {
  mutable count : int;
  mutable items : int list;
  operation : operation;
  operand : operand;
  test : int;
  test_true : int;
  test_false : int;
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
  | [ "*"; "old" ] -> (Mul, Old)
  | [ "+"; n ] -> (Sum, N (int_of_string n))
  | [ "*"; n ] -> (Mul, N (int_of_string n))
  | _ -> failwith ""

let parse_test line =
  let line = String.trim line in
  let list = String.split_on_char ' ' line in
  int_of_string (List.nth list 3)

let parse_if line =
  let line = String.trim line in
  let list = String.split_on_char ' ' line in
  int_of_string (List.nth list 5)

let skip_line ic = let _ = input_line ic in ()

let rec make_list line_num =
  if line_num = 0 then []
  else
    let () = skip_line input in
    let items = parse_items (input_line input) in
    let operation, operand = parse_operation (input_line input) in
    let test = parse_test (input_line input) in
    let test_true = parse_if (input_line input) in
    let test_false = parse_if (input_line input) in
    let _ = if line_num = 1 then "" else input_line input in
    { count = 0; items; operation; operand; test; test_true; test_false }
    :: make_list (line_num - 1)

let monkeys = Array.of_list (make_list 8)

let get_worry_level item = function
  | Mul, Old -> item * item
  | Sum, Old -> item + item
  | Mul, N n -> item * n
  | Sum, N n -> item + n

let rec remove element = function
  | [] -> []
  | h :: t -> if h = element then t else h :: remove element t


let rec step m =
  let rec aux = function
    | [] -> ()
    | h :: t ->
        let n = get_worry_level h (m.operation, m.operand) mod 9699690 in
        let () =
          if n mod m.test = 0 then
            monkeys.(m.test_true).items <- n :: monkeys.(m.test_true).items
          else monkeys.(m.test_false).items <- n :: monkeys.(m.test_false).items
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
  for i = 1 to 10000 do
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
let () = Printf.printf "Part 2 solution: %d.\n" result
