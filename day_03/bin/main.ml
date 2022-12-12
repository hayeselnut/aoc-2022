open Core

let letter_to_value c =
  match Char.is_uppercase c with
  | true -> Char.to_int c - Char.to_int 'A' + 1 + 26
  | false -> Char.to_int c - Char.to_int 'a' + 1

let calc_points filename =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:([], [], [], 0) ~f:(fun accum line_as_string ->
      let line = String.to_list line_as_string in
      match line with
      | [] -> accum
      | _ ->
        match accum with
        | [], _, _, s -> (line, [], [], s)
        | a, [], _, s -> (a, line, [], s)
        | a, b, _, s-> 
          let first_set = Char.Set.of_list a in
          let second_set = Char.Set.of_list b in
          let third_set = Char.Set.of_list line in
          let common_letter = Set.inter first_set second_set
            |> Set.inter third_set
            |> Set.to_list
            |> List.hd_exn in
          let value = letter_to_value common_letter in
          (* let () = printf "\n%d %d == common letter %d\n" (Set.length second_set) (Set.length first_set) value in *)
          ([], [], [], s + value)
    )
  );;

let (_, _, _, s) = calc_points "input.txt"
let () = printf "\n%d\n" @@ s
