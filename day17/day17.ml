open AdventStdLib

module Spinlock = struct
    let initial_state () = [|0|] (* Always start with an array having 0 in it *)

    let run_spinlock_step (arr: int array) (current_index: int) (spin_count: int) (next: int) : (int * int array) = 
        let array_length = Array.length arr in
        let new_current_position = ((current_index + spin_count) mod array_length) + 1 in
        let new_array_state = Arrays.insert_at arr (new_current_position) next in
        (new_current_position, new_array_state)

    let run_full_spinlock spin_count last_value_to_insert =
        let current_index = ref 0 in
        let next_number = ref 1 in
        let current_state = ref (initial_state()) in
        while !next_number <= last_value_to_insert do
            let (new_index, new_state) = run_spinlock_step !current_state !current_index spin_count !next_number in
            current_index := new_index;
            current_state := new_state;
            incr next_number
        done;
        !current_state
end


let input = 376 (* My input for the puzzle *)

let day17a () = 
    let final_result = Spinlock.run_full_spinlock input 2017 in
    let index_of_2017 = 
        Arrays.index_of 2017 final_result 
        |> Options.unwrap_exn
    in
    final_result.(index_of_2017 + 1)

let day17b () = 
    (* 50 million spinlock rounds takes forever. We just want the element at 
     * position 1. So let's do a "simulated" spinlock -- "sim" out the list length,
     * and check the index where we'd insert the new value, then increment list length *)
    let current_position = ref 0 in
    let value_at_position_1 = ref None in
    for current_value = 1 to 50_000_000 do
        (* The array length corresponds to the "current value", conveniently.
         * For example, when we're inserting "1", the array length is 1: [|0|].
         * Furthermore, the position we're going to insert into is 
         *      ((current + steps_forward) mod list_length) + 1
         * Which then becomes our new starting position for the next round.
         **)
        current_position := ((input + !current_position) mod current_value) + 1;

        (* If we're going to insert this value at position 1, it's our new 
         * most-recent-position-1-value *)
        if !current_position = 1 then
            value_at_position_1 := Some current_value
    done;
    Options.unwrap_exn !value_at_position_1

let () =
    day17a() |> Printf.printf "Day 17A: %d\n";
    day17b() |> Printf.printf "Day 17B: %d\n" 


