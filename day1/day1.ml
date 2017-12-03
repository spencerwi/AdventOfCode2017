let read_input () : int list =
    AdventStdLib.read_lines_from_file "input.txt"
	|> List.hd
    |> AdventStdLib.explode_str
    |> List.map int_of_string

let day1a input =
	let first_digit = List.hd input in
    let rec crunch = function 
        | []                                    -> 0
        | a::[] when a = first_digit            -> a 
        | a::[]                                 -> 0
        | a::b::rest when a = b                 -> a + (crunch (b::rest))
        | a::b::rest                            -> crunch (b::rest)
    in
    crunch input

let day1b input =
    let input_length = List.length input in
    let rotate_from idx =
        let adjusted_idx = (idx + (input_length / 2)) mod input_length in
        List.nth input adjusted_idx
    in
    let rec crunch idx = function 
        | []                                 -> 0
        | x::rest when x = (rotate_from idx) -> x + (crunch (idx+1) rest)
        | x::rest                            -> (crunch (idx+1) rest)
    in
    crunch 0 input

let () =
    let input = read_input() in
    let print_part = Printf.printf "Day 1%s: %d\n" in
    day1a input |> print_part "A" ;
    day1b input |> print_part "B"
