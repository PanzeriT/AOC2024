let split_lines = 
  String.split_on_char '\n'

let load_file =
  let read_file path = 
    In_channel.with_open_text path In_channel.input_all in
  read_file "d01p01.txt"
  |> split_lines

let process_strings strings =
  let split_and_pair s =
    let parts = s
                |> String.split_on_char ' '
                |> List.filter (fun x -> x <> "")
                |> List.map int_of_string
      in
      match parts with
      | [a; b] -> (a, b)
      | _ -> raise (Invalid_argument "Invalid input")
  in
    let pairs = List.map split_and_pair strings in
    List.split pairs

let get_lists content = 
 process_strings content

let calculate lists = 
  let (a, b) = lists  in
  let a' = List.sort compare a in
  let b' = List.sort compare b in
  List.combine a' b'
  |> List.map (fun (x,y) -> y - x |> abs)
  |> List.fold_left (+) 0

let solve () = 
  load_file
  |> get_lists
  |> calculate
  |> Printf.printf "D01P01: %d\n"