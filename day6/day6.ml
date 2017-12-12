type allocation = int array

let read_input () : allocation =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.hd
    |> AdventStdLib.String.split_on "[ \t]+"
    |> List.map int_of_string
    |> Array.of_list

(* Used to store the list of unique "allocation-lists" we've seen *)
module AllocationSet = Set.Make(struct
    type t = allocation
    let compare = compare
end);;
            
(* Reallocate blocks from memory, using Mancala movement,
 * starting from the memory slot with the most block *)
let reallocate memory = 
    (* Since we're modifying the array in place, let's make a "safe working copy" first *)
    let working_copy = Array.copy memory in
    (* Cache this up-front so we don't re-calculate each loop *)
    let array_length = Array.length working_copy in
    (* Find the largest-valued place in the array, and its value *)
    let index_of_max = AdventStdLib.Array.max_index working_copy in
    let max_value = Array.get working_copy index_of_max in
    (* Empty it out *)
    Array.set working_copy index_of_max 0;
    (* The number that was in that slot is how many block we have to distribute *)
    let amount_to_distribute = ref max_value in
    (* Starting with the *next* slot *)
    let current_index = ref (index_of_max + 1) in
    while !amount_to_distribute > 0 do
        (* Wrap around if needed *)
        if !current_index >= array_length then current_index := 0;
        (* Increment the current slot's contents *)
        AdventStdLib.Array.modify_element working_copy (fun x -> x + 1) !current_index;
        (* Now we have one fewer "piece" to give *)
        decr amount_to_distribute;
        (* Move to the next slot *)
        incr current_index
    done;
    (* Return the modified allocation array *)
    working_copy

(* We need to return the allocation that causes an infinite loop for use in 
   part b, along with the number of steps until infinite loop *)
let day6a (input : allocation) : (allocation * int) =
    let looper = ref None in
    let rec process allocations_seen allocation =
        (* If we've seen this allocation before, bail out *)
        if AllocationSet.mem allocation allocations_seen
        then (
            looper := Some allocation;
            0
        )
        else  (* Otherwise, this is one more possible unique reallocation *)
            let updated_seen_list = AllocationSet.add allocation allocations_seen in
            let next_allocation = reallocate allocation in
            1 + (process updated_seen_list next_allocation)
    in
    let steps_until_loop = process AllocationSet.empty input in
    (AdventStdLib.Option.unwrap_exn !looper, steps_until_loop)


let day6b (target: allocation) : int =
    let rec process allocation =
        if allocation = target
        then 1 (* At least one, in this case, since we need to shift once to get going *)
        else 
            let next_allocation = reallocate allocation in
            1 + (process next_allocation)
    in
    process (reallocate target)

let () =
    let input = read_input() in
    let print_result = Printf.printf "Day 6%s: %d\n" in
    let (looper, steps_until_loop) = day6a input in
    print_result "A" steps_until_loop;
    day6b looper |> print_result "B" ;


