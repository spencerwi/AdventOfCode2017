let read_input () =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.map (AdventStdLib.String.split_on "[ \t]+")
    |> List.map (List.map int_of_string)

let process_lines (checksum_func: int list -> int) (lines: int list list) : int =
    lines 
    |> List.map checksum_func
    |> List.fold_left (+) 0

let day1a (lines : int list list) : int =
    let list_min_max l = 
        let sorted_l = List.sort compare l in
        let min = List.hd sorted_l in
        let max = match AdventStdLib.List.last sorted_l with
            | Some a -> a
            | None -> failwith "List should not have been empty!"
        in (min, max)
    in
    let min_max_checksum line =
        let (min, max) = list_min_max line in
        max - min
    in
    process_lines min_max_checksum lines

let day1b (lines : int list list) : int =
    let find_divisible x others =
        let divisible_partner = 
            others 
            |> List.find_opt (fun y -> (x mod y) = 0)
        in match divisible_partner with
        | Some y -> Some (x, y)
        | None -> None
    in
    let divisibility_checksum line = 
        line 
        |> List.mapi (fun idx x -> 
            let others = AdventStdLib.List.all_but idx line in
            find_divisible x others 
        )
        |> AdventStdLib.List.flat_map (function
            | Some (a, b) -> [a / b]
            | None -> []
        )
        |> List.fold_left (+) 0 
    in
    process_lines divisibility_checksum lines

let () =
    let input = read_input() in
    let print_result = Printf.printf "Day 2%s: %d\n" in
    day1a input |> print_result "A" ;
    day1b input |> print_result "B"

