open Core

let manhat_dist x1 y1 x2 y2 = abs(x1-x2) + abs(y1-y2)

let calculate_new_t ((hx, hy), (tx, ty)) =
  match manhat_dist hx hy tx ty with
  | 0 | 1 -> (tx, ty) (* do nothing, on top of each other *)
  | 2 ->
    (* diagonal 2 allowed, but not linear 2 *)
    if hx = tx || hy = ty
    then (* linear *)
      match hx-tx, hy-ty with
      | 2, 0 -> (tx+1, ty)
      | -2, 0 -> (tx-1, ty)
      | 0, 2 -> (tx, ty+1)
      | 0, -2 -> (tx, ty-1)
      | _, _ -> 
        let () = printf "uh oh linear" in
        (-1, -1)
    else (tx, ty) (* diagonal *)
  | _ -> match hx-tx, hy-ty with
          | 2, 1 | 2, -1 -> (tx+1, hy)
          | -2, 1 | -2, -1 -> (tx-1, hy)
          | 1, 2 | -1, 2 -> (hx, ty+1)
          | 1, -2 | -1, -2 -> (hx, ty-1)
          | _, _ ->
            let () = printf "uh oh" in
            (-1, -1)

let advance accum magnitude offset_x offset_y =
  List.init magnitude ~f:Fn.id
  |> List.fold ~init:accum ~f:(fun accum _ ->
      let ((hx, hy), (tx, ty), t_positions) = accum in
    let new_head = (hx + offset_x), (hy + offset_y) in
    let new_tail = calculate_new_t (new_head, (tx,ty)) in
    (new_head, new_tail, new_tail :: t_positions)
    )

let calc_answer filename =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:((0, 0), (0, 0), []) ~f:(fun accum line ->
      let split_data = String.split ~on:' ' line in
      match split_data with
      | [] | [_] -> accum
      | direction :: magnitude_string :: _ ->
        let magnitude = Int.of_string magnitude_string in
        match direction with
        | "R" -> advance accum magnitude 1 0
        | "L" -> advance accum magnitude (-1) 0
        | "U" -> advance accum magnitude 0 1
        | "D" -> advance accum magnitude 0 (-1)
        | _ -> accum
    )
  );;

let _,_, t_positions = calc_answer "input.txt"

let unique =
  List.map t_positions ~f:(fun (x, y) -> (Int.to_string x) ^ " " ^ (Int.to_string y))
  |> String.Set.of_list
let () = printf "\n%d\n" @@ Set.length unique
