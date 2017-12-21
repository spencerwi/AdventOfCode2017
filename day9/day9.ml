(* Convenience alias for manipulating refs *)
let (+=) (ref: int ref) (amount: int) : unit = 
    ref := !ref + amount

let read_input () : string array =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.hd
    |> AdventStdLib.Strings.explode_str
    |> Array.of_list (* Arrays are better-suited for arbitrary-index access *)


(* Some convenience aliases for readability *)
let move_forward (index: int ref) = incr index
let toggle_garbage (is_garbage : bool ref) = is_garbage := not !is_garbage
let count_group (score: int ref) (group_value: int ref) = 
    score += !group_value;
    decr group_value

let day9a input =
    (* Iterate over each character in the input and handle it appropriately *)
    let score = ref 0 in
    let group_value = ref 0 in
    let index = ref 0 in
    let is_garbage = ref false in
    let input_length = Array.length input in
    while !index < input_length do
        let next_char = input.(!index) in
        begin if !is_garbage then
            begin match next_char with
            | "!" -> move_forward index
            | ">" -> toggle_garbage is_garbage 
            | _   -> ignore()
            end
        else
            begin match next_char with
            | "{" -> incr group_value
            | "<" -> toggle_garbage is_garbage 
            | "}" -> count_group score group_value
            | _   -> ignore()
            end
        end;
        move_forward index
    done;
    !score

let day9b input =
    (* Iterate over input, using similar rules, but this time counting garbage 
     * chars instead of scoring groups *)
    let garbage_char_count = ref 0 in
    let index = ref 0 in
    let is_garbage = ref false in
    let input_length = Array.length input in
    while !index < input_length do
        let next_char = input.(!index) in
        begin if !is_garbage then
            begin match next_char with
            | "!" -> move_forward index
            | ">" -> toggle_garbage is_garbage 
            | _   -> incr garbage_char_count
            end
        else
            begin match next_char with
            | "<" -> toggle_garbage is_garbage 
            | _   -> ignore()
            end
        end;
        move_forward index
    done;
    !garbage_char_count

let () =
    let input = read_input() in
    day9a input |> Printf.printf "Day 9A: %d\n";
    day9b input |> Printf.printf "Day 9B: %d\n"
