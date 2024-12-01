let calculate lists =
  let a, b = lists in
  let a' = List.sort compare a in
  let b' = List.sort compare b in
  List.combine a' b'
  |> List.map (fun (x, y) -> y - x |> abs)
  |> List.fold_left ( + ) 0
