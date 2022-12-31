open Core

type line_type =
| Cd_up
| Cd
| Ls
| Dir
| File_details

(* Use a map and the key is the path of the directory *)
(* value is a tuple of int * string representing size and name *)

let is_dir_command = String.is_prefix ~prefix:"dir"

let is_cd_up_command = String.equal "$ cd .."
let is_ls_command = String.equal "$ ls"
let is_cd_command = String.is_prefix ~prefix:"$ cd"

let get_dir_name = String.substr_replace_first ~pattern:"dir " ~with_:""
let get_cd_name = String.substr_replace_first ~pattern:"$ cd " ~with_:""

let get_parent_dir_name dir =
  (String.chop_suffix_exn dir ~suffix:"/"
  |> String.rsplit2_exn ~on:'/'
  |> fst) ^ "/"

let get_line_type line =
  if is_cd_up_command line then Cd_up
  else if is_cd_command line then Cd
  else if is_ls_command line then Ls
  else if is_dir_command line then Dir
  else File_details

let calc_answer filename =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:("/", String.Map.of_alist_multi []) ~f:(fun (curr_directory, files) line ->
      match get_line_type line with
      | Cd_up -> (get_parent_dir_name curr_directory, files)
      | Cd ->
        (curr_directory ^ get_cd_name line ^ "/", files)
      | Ls -> (curr_directory, files)
      | Dir ->
        let full_dir = curr_directory ^ get_dir_name line ^ "/" in
        (curr_directory, Map.add_exn files ~key:full_dir ~data:[])
      | File_details ->
        match String.split line ~on:' ' with
        | [size; filename] ->
          let () = print_endline ("Found " ^ filename ^ " file with size " ^ size) in
          (curr_directory, Map.add_multi files ~key:curr_directory ~data:(Int.of_string size, filename))
        | _ -> 
          let () = print_endline "=======IMPOSSIBLE! file_detail=====" in
          (curr_directory, files)
    )
  )
;;

let (_curr_directory, files) = calc_answer "input.txt"

let () = print_endline ""

let () = Map.iteri files ~f:(fun ~key ~data ->
  print_string (key ^ ": ");
  List.iter data ~f:(fun (size, filename) -> print_string ("(" ^ Int.to_string size ^ ", " ^ filename ^ ") "));
  print_endline "";
)
;;

let () = print_endline ""

let exclusive_directory_sizes =
  Map.map files ~f:(fun fs -> 
    List.fold fs ~init:0 ~f:(fun accum (size, _) -> accum + size)
  )
;;

let () = Map.iteri exclusive_directory_sizes ~f:(fun ~key ~data ->
  print_endline (key ^ ": " ^ Int.to_string data)
)
;;

let () = print_endline ""

let accumulative_directory_sizes =
  Map.mapi exclusive_directory_sizes ~f:(fun ~key:curr_key ~data:_ ->
    Map.fold exclusive_directory_sizes ~init:0 ~f:(fun ~key ~data accum ->
        match String.is_prefix key ~prefix:curr_key with
        | true -> accum + data
        | false -> accum
      )
    )
;;

let () = print_endline ""
let () = Map.iteri accumulative_directory_sizes ~f:(fun ~key ~data ->
  print_endline (key ^ ": " ^ Int.to_string data)
)
;;
let () = print_endline ""
let filtered = Map.filter accumulative_directory_sizes ~f:(fun size -> size <= 100000)
let () = Map.iteri filtered ~f:(fun ~key ~data ->
  print_endline (key ^ ": " ^ Int.to_string data)
)
;;
let sum = Map.fold filtered ~init:0 ~f:(fun ~key:_ ~data accum ->
  accum + data
)
;;
let () = print_endline ""
let () = print_endline (Int.to_string sum)

(* PART TWO *)

let need_to_delete = (Map.find_exn accumulative_directory_sizes "/") - 40000000

let () = print_endline "\n\nNEED TO DELETE: "
let () = print_endline (Int.to_string need_to_delete)

let filtered_can_delete = Map.filter accumulative_directory_sizes ~f:(fun size -> size >= need_to_delete)

let smallest_size = Map.data filtered_can_delete |> List.min_elt ~compare:Int.compare

let () = print_endline ""
let () = Map.iteri filtered_can_delete ~f:(fun ~key ~data ->
  print_endline (key ^ ": " ^ Int.to_string data)
)
;;

let () = Option.iter smallest_size ~f:(fun elem -> print_endline (Int.to_string elem))
