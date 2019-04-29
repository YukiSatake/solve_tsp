let _ =
  Tsplib.read_from_file "data/a280.tsp";
  Tst.image_of_tsp []
  |> fun (size, view, image) ->
     Tsp.render size view image
