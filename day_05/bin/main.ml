open Core

let crates = String.Map.of_alist_exn
[
    ("0", ["~"])
  ; ("1", ["W"; "P"; "G"; "Z"; "V"; "S"; "B"])
  ; ("2", ["F"; "Z"; "C"; "B"; "V"; "J"])
  ; ("3", ["C"; "D"; "Z"; "N"; "H"; "M"; "L"; "V"])
  ; ("4", ["B"; "J"; "F"; "P"; "Z"; "M"; "D"; "L"])
  ; ("5", ["H"; "Q"; "B"; "J"; "G"; "C"; "F"; "V"])
  ; ("6", ["B"; "L"; "S"; "T"; "Q"; "F"; "G"])
  ; ("7", ["V"; "Z"; "C"; "G"; "L"])
  ; ("8", ["G"; "L"; "N"])
  ; ("9", ["C"; "H"; "F"; "J"])
]


(* let crates = String.Map.of_alist_exn
[
    ("0", ["~"])
  ; ("1", ["N"; "Z"])
  ; ("2", ["D"; "C"; "M"])
  ; ("3", ["P"])
]
;; *)

let move_crate ~from ~to_ crates =
  match Map.find_multi crates from with
  | moving :: new_from_crates -> 
    let () = print_endline ("Found crate [" ^ moving ^ "]") in
    let new_to_crates = moving :: Map.find_multi crates to_ in
    Map.set crates ~key:from ~data:new_from_crates
    |> Map.set ~key:to_ ~data:new_to_crates
  | _ ->
    let () = print_endline "====IMPOSSIBLE!=====" in
    crates
;;

let rec move ~n ~from ~to_ crates =
  match n with
  | 0 -> crates
  | _ -> move ~n:(n-1) ~from ~to_ (move_crate ~from ~to_ crates)
;;

let calc_answer filename =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:crates ~f:(fun accum line ->
      let split =
        line
        |> String.substr_replace_all ~pattern:"move " ~with_:""
        |> String.substr_replace_all ~pattern:" from " ~with_:";"
        |> String.substr_replace_all ~pattern:" to " ~with_:";"
        |> String.split ~on:';'
      in
      match split with
      | [n; from; to_] -> 
          move ~n:(Int.of_string n) ~from ~to_:"0" accum
          |> move ~n:(Int.of_string n) ~from:"0" ~to_
      | _ -> accum
    )
  )
;;

let final_crates = calc_answer "input.txt"

let message = Map.fold final_crates ~init:"" ~f:(fun ~key:_ ~data accum ->
    accum ^ List.hd_exn data
  )

let () = print_endline ""
let () = print_endline message
