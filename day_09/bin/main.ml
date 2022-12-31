open Core

let manhat_dist x1 y1 x2 y2 = abs(x1-x2) + abs(y1-y2)
let is_diagonal x1 y1 x2 y2 = abs(x1-x2) = 1 && abs(y1-y2) = 1

let calculate_new_t ((hx, hy), (tx, ty)) =
  match manhat_dist hx hy tx ty with
  | 0 | 1 -> (tx, ty) (* do nothing, on top of each other *)
  | _ ->
    if is_diagonal hx hy tx ty
    then (tx, ty)
    else if hx = tx && hy <> ty (* x equal, move along a x direction *)
    then match hy < ty with
      | true -> tx, ty-1
      | false -> tx, ty+1
    else if hx <> tx && hy = ty (* y equal, move along a y direction *)
    then match hx < tx with
      | true -> tx-1, ty
      | false -> tx+1, ty
    else
      match hx < tx, hy < ty with
      | true, true -> tx-1, ty-1
      | true, false -> tx-1, ty+1
      | false, true -> tx+1, ty-1
      | false, false -> tx+1, ty+1

let advance accum magnitude offset_x offset_y =
  List.init magnitude ~f:Fn.id
  |> List.fold ~init:accum ~f:(fun accum _ ->
      let (positions, t_positions) = accum in
      let new_tail, new_positions = List.fold_map positions ~init:(-1000, -1000) ~f:(
        fun (prev_x, prev_y) (x, y) ->
          let new_pos = 
            match prev_x with
            | (-1000) -> (x + offset_x, y + offset_y)
            | _ -> calculate_new_t ((prev_x, prev_y), (x, y))
          in
          (new_pos, new_pos)
        )
      in
      (new_positions, (new_tail :: t_positions))
    )

let starting_positions = [
    0,0 (* H *)
  ; 0,0 (* 1 *)
  ; 0,0 (* 2 *)
  ; 0,0 (* 3 *)
  ; 0,0 (* 4 *)
  ; 0,0 (* 5 *)
  ; 0,0 (* 6 *)
  ; 0,0 (* 7 *)
  ; 0,0 (* 8 *)
  ; 0,0 (* 9 *)
]

let calc_answer filename =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:(starting_positions, []) ~f:(fun accum line ->
      let split_data = String.split ~on:' ' line in
      match split_data with
      | [] | [_] -> accum
      | direction :: magnitude_string :: _ ->
        let magnitude = Int.of_string magnitude_string in
        let () = print_endline ("moving in " ^ direction) in
        match direction with
        | "R" -> advance accum magnitude 1 0
        | "L" -> advance accum magnitude (-1) 0
        | "U" -> advance accum magnitude 0 1
        | "D" -> advance accum magnitude 0 (-1)
        | _ -> accum
    )
  );;

let _, t_positions = calc_answer "input.txt"

let unique =
  List.map t_positions ~f:(fun (x, y) -> (Int.to_string x) ^ " " ^ (Int.to_string y))
  |> String.Set.of_list
let () = printf "\n%d\n" @@ Set.length unique
