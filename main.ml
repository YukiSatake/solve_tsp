let _ =
  Tsplib.read_from_file "data/a280.tsp"
  |> Tsplib.to_tsp
  |> NeuralGASolver.(solve default_configure)
