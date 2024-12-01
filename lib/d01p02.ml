let item_count item list = List.filter (fun x -> x == item) list |> List.length

let calculate lists =
  let a, b = lists in
  List.map (fun v -> v * item_count v b) a |> List.fold_left ( + ) 0
