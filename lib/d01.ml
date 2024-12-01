let split_lines = String.split_on_char '\n'

let load_file =
  let read_file path = In_channel.with_open_text path In_channel.input_all in
  read_file "d01.txt" |> split_lines

let process_strings strings =
  let split_and_pair s =
    let parts =
      s |> String.split_on_char ' '
      |> List.filter (fun x -> x <> "")
      |> List.map int_of_string
    in
    match parts with
    | [ a; b ] -> (a, b)
    | _ -> raise (Invalid_argument "Invalid input")
  in
  let pairs = List.map split_and_pair strings in
  List.split pairs

let solve f = load_file |> process_strings |> f
let solve_part1 = solve D01p01.calculate
let solve_part2 = solve D01p02.calculate
