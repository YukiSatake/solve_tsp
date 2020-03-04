open Core

type gene = dist list
and dist = int

type generation = gene list

type assessment = (gene * float) list

type config = { number_of_candidates: int; (* 各世代での候補遺伝子数 *)
                limit_of_generation: int; (* 世代の上限 *)
                p_mutate: float; (* 突然変異確率 *)
                p_cross: float;  (* 交叉確率 *)
                p_select: float; (* 選択確率 *)
              }

let _ = Random.init 1919 (* 再現性を確保するためSeed値を固定 *)
     (* Random.self_init () *)

let default_configure : config = { number_of_candidates = 4;
                                   limit_of_generation = 10;
                                   p_mutate = 0.1;
                                   p_cross  = 0.2;
                                   p_select = 0.7;
                                 }

let str_of_gene gene = String.concat ~sep:";"
                       @@ List.map ~f:string_of_int gene
                       |> fun s -> "[" ^ s ^ "]"

let str_of_genes genes = String.concat ~sep:"\n"
                         @@ List.map ~f:str_of_gene genes

let best_gene_of (assessment: assessment) = 
  assessment
  |> List.fold_left
       ~f:(fun (acc, cur_max) (gene, sc) ->
         if cur_max < sc then (gene, sc) else (acc, cur_max)) ~init:([], 0.)
  |> fst
  

let gen_initial_solution num_of_cities =
  List.init num_of_cities ~f:(fun i -> Random.int @@ num_of_cities - i)

let select assessment =
  let sorted = 
    List.sort ~compare:(fun (_, score) (_, score') -> compare score' score)
      assessment in
  List.nth sorted 0 |> fun (gene, _) -> gene

let cross _  = assert false
let mutate config assessment = 
  let target = fst @@ List.nth assessment (Random.int config.population) in
  

let ga config tsp first_generation =
  let next_generation_of (assessment: assessment) =
    List.init config.number_of_candidates ~f:(fun _ ->
        let r = Random.float 1. in
        (* TODO: This should be fixed. *)
        if r < config.p_select then select assessment
        else if r < config.p_select +. config.p_cross then cross assessment
        else mutate assessment
      ) in
  let eval_gene (tsp: Tsp.t) (gene: gene) =
    let initial = let n = List.hd_exn gene in
                  0., (Map.Poly.find_exn tsp n) in
    List.tl_exn gene
    |> List.fold_left ~init:initial ~f:(fun (score, (x, y)) n ->
           let x', y' = Map.Poly.find_exn tsp n in
           let score' = (x -. x') *. (x -. x') +. (y -. y') *. (y -. y') |> sqrt in
           (score +. score', (x', y'))
         )
    |> fun (score, _) -> gene, score
  in
  let rec inner cnt generation =
    let assessment: assessment = List.map ~f:(eval_gene tsp) generation in
    Printf.printf "********************* %d-th Generation: \n" cnt;
    print_endline @@ str_of_genes generation;
    if cnt >= config.limit_of_generation then assessment
    else
      let next_generation = next_generation_of assessment in
      inner (cnt + 1) next_generation
  in inner 0 first_generation
  

let solve (config: config) (tsp: Tsp.t) =
  let num_of_cities = Map.Poly.length tsp in
  let init = 
    List.init config.number_of_candidates 
      ~f:(fun _ -> gen_initial_solution num_of_cities) in
  print_string @@ str_of_genes init;
  let last_generation = ga config tsp init in
      print_endline "******************* Answer:";
      last_generation
      |> best_gene_of
      |> str_of_gene
      |> print_endline
