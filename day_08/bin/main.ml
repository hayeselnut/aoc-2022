open! Core

type visibility =
| Visible
| Not_visible

let get_map filename =
  In_channel.read_lines filename
  |> List.map ~f:String.to_list (* char list list *)
  |> List.map ~f:(List.map ~f:Char.to_int) (* int list list *)
  |> List.map ~f:(List.map ~f:(fun ascii_value -> ascii_value - (Char.to_int '0'))) (* int list list *)
  |> List.map ~f:(List.map ~f:(fun height -> (height, Not_visible))) (* (int * visibility) list list *)
;;

let scan map = List.map map ~f:(List.folding_map ~init:(-1) ~f:(fun max (height, vis) ->
  match max < height with
  | true -> (height, (height, Visible))
  | false -> (max, (height, vis))
))

let count_visible = List.fold ~init:0 ~f:(fun accum row ->
  accum + List.fold row ~init:0 ~f:(fun accum (_, vis) ->
    match vis with
    | Visible -> accum + 1
    | Not_visible -> accum
  )
)

let input_map = get_map "input.txt"

let visible_trees = input_map
  |> scan
  |> List.map ~f:List.rev
  |> scan
  |> List.map ~f:List.rev
  |> List.transpose_exn
  |> scan
  |> List.map ~f:List.rev
  |> scan
  |> List.map ~f:List.rev
  |> List.transpose_exn
  |> count_visible

let () = print_endline ""
let () = print_endline ("Visible trees: " ^ (Int.to_string visible_trees))
let () = print_endline ""

let count_until ~max = List.fold_until ~init:0 ~f:(fun count x ->
    if x < max
    then Continue (count + 1)
    else Stop (count + 1))
  ~finish:(fun counts -> counts)

let count_visible_left map = List.map map ~f:(
  List.folding_map ~init:[] ~f:(fun prevs (height, _) ->
    (* return acc then left_count *)
    match prevs with
    | [] -> [height], 0 (* starting *)
    | _ -> height :: prevs, count_until ~max:height prevs
  )
)

let count_left = count_visible_left input_map
let count_right = input_map
  |> List.map ~f:List.rev
  |> count_visible_left
  |> List.map ~f:List.rev

let count_down = input_map
  |> List.transpose_exn
  |> count_visible_left
  |> List.transpose_exn

let count_up = input_map
  |> List.transpose_exn
  |> List.map ~f:List.rev
  |> count_visible_left
  |> List.map ~f:List.rev
  |> List.transpose_exn

let multiply_maps =
  List.map2_exn ~f:(List.map2_exn ~f:Int.( * ))

let print_map map = 
  print_endline "";
  List.iter map ~f:(fun row ->
    List.iter row ~f:(fun num -> print_string ("[" ^ (Int.to_string num) ^ "] "));
    print_endline "";
  );
  print_endline ""

let () = print_map count_left
let () = print_map count_right
let () = print_map count_down
let () = print_map count_up

let scenic_products = count_left
  |> multiply_maps count_right
  |> multiply_maps count_down
  |> multiply_maps count_up

let () = print_map scenic_products

let max_scenic_product = scenic_products
  |> List.map ~f:(List.max_elt ~compare:Int.compare)
  |> List.map ~f:(Option.value ~default:0)
  |> List.max_elt ~compare:Int.compare
  |> Option.value ~default:0

let () = print_endline ("Max scenic product: " ^ Int.to_string max_scenic_product)
