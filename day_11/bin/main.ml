open! Core

let product = 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19

module Monkey = struct
  type t = {
      name: int
    ; items : int list
    ; operation: int -> int
    ; test: int -> int
    ; inspected: int
  } [@@deriving fields]

  let create input_list : t =
    let parse_op op_str =
      match op_str with
      | "* old" -> (fun old -> (old * old) % product)
      | _ ->
        match String.split op_str ~on:' ' with
        | ["+"; n] -> Int.( + ) (Int.of_string n)
        | ["*"; n] -> Int.( * ) (Int.of_string n)
        | _ ->
          let () = print_endline "===imposiible op str parse===" in
          Int.(+) 1
    in
    let parse_test test_str throw_t_str throw_f_str x = 
      match x % (Int.of_string test_str) with
      | 0 -> Int.of_string throw_t_str
      | _ -> Int.of_string throw_f_str
    in
    match input_list with
    | [name_str; items_str; op_str; test_str; throw_t_str; throw_f_str] -> 
      {
        name = Int.of_string name_str
      ; items = 
        String.substr_replace_all items_str ~pattern:", " ~with_:","
        |> String.split ~on:','
        |> List.map ~f:Int.of_string
      ; operation = parse_op op_str
      ; test = parse_test test_str throw_t_str throw_f_str
      ; inspected = 0
      }
    | _ ->
      let () = print_endline "====IMPOSSIBLE!====" in
      {
        name = -1
      ; items = []
      ; operation = Int.(+) 1
      ; test = (fun _ -> 0)
      ; inspected = -1
      }
  ;;

  let clear_items t =
    {
      name = t.name
      ; items = []
      ; operation = t.operation
      ; test = t.test
      ; inspected = ((List.length t.items) + t.inspected)
    }
  ;;

  let add_item t new_worry =
    {
      name = t.name
      ; items = t.items @ [new_worry]
      ; operation = t.operation
      ; test = t.test
      ; inspected = t.inspected
    }
  ;;

  let to_tuple t = t.name, t

  let items t = t.items

  let inspected t = t.inspected

  let print t =
    let items =
      t.items
      |> List.map ~f:Int.to_string
      |> String.concat ~sep:", "
    in
    ignore t.operation;
    ignore t.test;
    printf "Monkey %d (%d): [%s] \n" t.name t.inspected items
end

let get_monkeys filename =
  In_channel.read_all filename
  |> String.substr_replace_all ~pattern:"\n\n" ~with_:"#"
  |> String.split ~on:'#'
  |> List.map ~f:(String.substr_replace_all ~pattern:"Monkey " ~with_:"")
  |> List.map ~f:(String.substr_replace_all ~pattern:":\n" ~with_:"~")
  |> List.map ~f:(String.substr_replace_all ~pattern:"  Starting items: " ~with_:"")
  |> List.map ~f:(String.substr_replace_all ~pattern:"  Operation: new = old " ~with_:"")
  |> List.map ~f:(String.substr_replace_all ~pattern:"  Test: divisible by " ~with_:"")
  |> List.map ~f:(String.substr_replace_all ~pattern:"    If true: throw to monkey " ~with_:"")
  |> List.map ~f:(String.substr_replace_all ~pattern:"    If false: throw to monkey " ~with_:"")
  |> List.map ~f:(String.substr_replace_all ~pattern:"\n" ~with_:"~")
  |> List.map ~f:(String.split ~on:'~')
  |> List.map ~f:Monkey.create
  |> List.map ~f:Monkey.to_tuple
  |> Int.Map.of_alist_exn

let monkeys = get_monkeys "input.txt"
let () = print_endline ""
let () = Map.iter monkeys ~f:Monkey.print

let play_turn name monkeys =
  let m = Map.find_exn monkeys name in
  let thrown_items = List.fold (Monkey.items m) ~init:monkeys ~f:(fun accum worry ->
    let new_worry = (m.operation worry) % product in
    let res_monkey_name = m.test new_worry in
    let res_monkey = Map.find_exn accum res_monkey_name in
    Map.set accum ~key:res_monkey_name ~data:(Monkey.add_item res_monkey new_worry)
  )
  in
  Map.set thrown_items ~key:name ~data:(Monkey.clear_items m)
;;

let play_round monkeys = Map.fold monkeys ~init:monkeys ~f:(fun ~key ~data:_ acc -> play_turn key acc)

let rec play_rounds monkeys ~rounds =
  if rounds = 0 then monkeys else
  let new_monkeys = play_round monkeys in
  match 10001-rounds with
  | 1 | 20 | 1000 | 10000 ->
    let () = print_endline "" in
    let () = printf "[ Round %d ]\n" (10001-rounds) in
    let () = Map.iter new_monkeys ~f:Monkey.print in
    play_rounds new_monkeys ~rounds:(rounds - 1)
  | _ -> play_rounds new_monkeys ~rounds:(rounds - 1)

let final_monkeys = play_rounds monkeys ~rounds:10000

let inspected_sorted =
  Map.to_alist final_monkeys
  |> List.map ~f:snd
  |> List.map ~f:Monkey.inspected
  |> List.sort ~compare:Int.compare
  |> List.rev

let () = print_endline "\nInspected sorted:"
let () = List.iter inspected_sorted ~f:(fun elem -> print_endline (Int.to_string elem))

let monkey_business =
  List.take inspected_sorted 2
  |> List.fold ~init:1 ~f:(fun acc elem -> acc * elem)

let () = printf "Monkey business: %d\n" monkey_business
