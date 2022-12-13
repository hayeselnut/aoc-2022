open Core

let ans = In_channel.read_lines "input.txt"
|> List.hd_exn
|> String.to_list
|> List.fold_until
    ~init:[]
    ~f:(fun last_letters elem ->
      if List.length last_letters < 14 - 1
      then Continue (elem :: last_letters)
      else
        match Char.Set.of_list (elem :: (List.take last_letters 13)) |> Set.length with
        | 14 -> Stop (elem :: last_letters)
        | _ -> Continue (elem :: last_letters))
    ~finish:Fn.id
  |> List.length
;;

let () = printf "\n%d\n" @@ ans
