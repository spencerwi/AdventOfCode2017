let read_input () : int array =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.map int_of_string
    |> Array.of_list
            
(* We use an array for its mutability since this is more efficient than creating 
    updated lists each time. To prevent messing up the array for other consumers
    (e.g. multiple days) we make a "working copy" before modifying it *)
let process_instructions (offsets : int array) (incrementer : int -> int) : int =
    let working_copy_of_offsets = Array.copy offsets in
    let maze_length = Array.length working_copy_of_offsets in
    let jump_count = ref 0 in
    let current_offset = ref 0 in
    while !current_offset < maze_length do
        let jump_amount = Array.get working_copy_of_offsets !current_offset in
        let incremented_offset = incrementer jump_amount in

        (* Take note! Mutation here! *)
        Array.set working_copy_of_offsets !current_offset incremented_offset;

        current_offset := !current_offset + jump_amount;
        incr jump_count
    done;
    !jump_count


let day5a (offsets : int array) : int =
    let always_increment = (+) 1 in
    process_instructions offsets always_increment

let day5b (offsets : int array) : int =
    let decrement_if_three_or_more x = 
        if x >= 3 
        then x - 1 
        else x + 1
    in
    process_instructions offsets decrement_if_three_or_more


let () =
    let input = read_input() in
    let print_result = Printf.printf "Day 5%s: %d\n" in
    day5a input |> print_result "A" ;
    day5b input |> print_result "B" ;

