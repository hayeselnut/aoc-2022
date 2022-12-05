open Core

let calc_win_points opp me = match Char.to_int me - Char.to_int opp with
    | 23      -> 3 (* draw *)
    | 21 | 24 -> 6 (* win *)
    | 22 | 25 -> 0 (* lose *)
    | _ -> 0

let calc_my_action me = match me with
    | 'X' -> 1
    | 'Y' -> 2
    | 'Z' -> 3
    | _ -> 0

let calc_round_points round_string =
  let opp = String.get round_string 0 in
  let me = String.get round_string 2 in
  let win_points = calc_win_points opp me in
  let my_action = calc_my_action me in
  win_points + my_action

let calc_points filename =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun sum line ->
      sum + calc_round_points line
    )
  );;

let () = printf "\n%d\n" @@ calc_points "input.txt"

(* PART 2 *)

let calc_result_points me = match me with
    | 'X' -> 0
    | 'Y' -> 3
    | 'Z' -> 6
    | _ -> 0

let calc_resulting_action opp me =
  match (opp, me) with
    | ('A','X') -> 3
    | ('B','X') -> 1
    | ('C','X') -> 2

    | ('A','Y') -> 1
    | ('B','Y') -> 2
    | ('C','Y') -> 3

    | ('A','Z') -> 2
    | ('B','Z') -> 3
    | ('C','Z') -> 1
    | _ -> 0

let calc_part_2 round_string =
  let opp = String.get round_string 0 in
  let me = String.get round_string 2 in
  let win_points = calc_result_points me in
  let my_action = calc_resulting_action opp me in
  win_points + my_action

let calc_part_2_points filename =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun sum line ->
      sum + calc_part_2 line
    )
  );;

let () = printf "\n%d\n" @@ calc_part_2_points "input.txt"
