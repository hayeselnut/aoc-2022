open Core

let calc_answer filename =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:(1, 1, []) ~f:(fun (cycle, x, states) line ->
      let split_data = String.split ~on:' ' line in
      match split_data with
      | [] -> (cycle, x, states)
      | hd :: tl -> match hd with
        | "noop" ->
          (cycle + 1, x, (cycle, x) :: states)
        | "addx" ->
          let v = List.hd_exn tl |> Int.of_string in
          (cycle + 2, x + v, (cycle + 1, x) :: (cycle, x) :: states)
        | _ -> (cycle, x, states)
    )
  );;

let (_, _, states) = calc_answer "input.txt";;

let important_states = List.filter states ~f:(fun (cycle, _) ->
  match cycle with
  | 20 | 60 | 100 | 140 | 180 | 220 -> true
  | _ -> false
);;

List.iter important_states ~f:(fun (cycle, x) -> printf "[ %d %d ] " cycle x);;

let ans = List.fold important_states ~init:0 ~f:(fun accum (cycle, x) ->
  accum + cycle * x
);;

let () = printf "\n === %d ===\n" ans;;


printf "\n";

List.iter (List.rev states) ~f:(fun (cycle, x) ->
  match cycle with
  | 40 | 80 | 120 | 160 | 200 -> printf "\n"
  | _ -> printf "";

  let modded = cycle % 40 in
  match x <= modded && modded <= x+2 with
  | true -> printf "#"
  | false -> printf "."
);
