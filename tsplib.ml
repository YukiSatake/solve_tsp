open Core

type t = {name: string;
               comment: string; 
               ptype: ptype;
               dimension: int;
               edge_weight_type: etype;
               nodes: node list
              }
and ptype = TSP | ATSP | TOUR
and etype = EUC_2D
and node  = int * float * float

let str_to_ptype = function "TSP" -> TSP | "ATSP" -> ATSP | "TOUR" -> TOUR | _ -> failwith ""
let str_to_etype = function "EUC_2D" -> EUC_2D | _ -> failwith ""

let tsplib name comment ptype dim etype nodes =
  {name=name; comment=comment; ptype=ptype; dimension=dim; edge_weight_type=etype; nodes=nodes}

let elim_spaces = String.filter ~f:(function ' ' -> false | _ -> true)

let name_of (res, ls) = 
  let l, ls = List.hd_exn ls, List.tl_exn ls in
  print_endline l;
  Scanf.sscanf (elim_spaces l) "NAME:%s" (fun s -> res s, ls)

let comment_of (res, ls) =
  let l, ls = List.hd_exn ls, List.tl_exn ls in
  print_endline l;
  Scanf.sscanf (elim_spaces l) "COMMENT:%s" (fun s -> res s, ls)

let ptype_of (res, ls) =
  let l, ls = List.hd_exn ls, List.tl_exn ls in
  print_endline l;
  Scanf.sscanf (elim_spaces l) "TYPE:%s" (fun s -> res (str_to_ptype s), ls)

let dimension_of (res, ls) =
  let l, ls = List.hd_exn ls, List.tl_exn ls in
  print_endline l;
  Scanf.sscanf (elim_spaces l) "DIMENSION:%d" (fun d -> res d, ls)
let etype_of (res, ls) =
  let l, ls = List.hd_exn ls, List.tl_exn ls in
  print_endline l;
  Scanf.sscanf (elim_spaces l) "EDGE_WEIGHT_TYPE:%s" (fun s -> res (str_to_etype s), ls)

let nodes_of (res, ls) =
  let l, ls = List.hd_exn ls, List.tl_exn ls in
  let rec inner acc = function
    | [] -> List.rev acc
    | l::[] when l = "EOF" -> List.rev acc
    | l::ls ->
       let nxy = String.split ~on:' ' l |> List.filter ~f:(function "" -> false | _ -> true) in
       let node = (List.nth_exn nxy 0 |> int_of_string, 
                   List.nth_exn nxy 1 |> float_of_string, 
                   List.nth_exn nxy 2 |> float_of_string) in
       inner (node::acc) ls
  in
  if l <> "NODE_COORD_SECTION" then failwith "Invalid format"
  else inner [] ls |> res

let read_from_file filename =
  let r = In_channel.read_lines filename in
  try
    name_of (tsplib, r)
    |> comment_of
    |> ptype_of
    |> dimension_of 
    |> etype_of
    |> nodes_of
  with e -> 
    let msg = Exn.to_string e in
    let stack = Printexc.get_backtrace () in
    Printf.eprintf "Invalid format: %s%s\n" msg stack;
    raise e

let to_tsp (tsplib: t) = 
  tsplib.nodes
  |> List.fold_left ~f:(fun acc (n, x, y) ->
         Map.Poly.add_exn acc ~key:n ~data:(x, y)) ~init:Map.Poly.empty
