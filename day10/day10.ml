module CircularArray = struct 
    include Array

    let get (arr: 'a array) (index: int) : 'a =
        let arr_length = Array.length arr in
        let rotated_index = index mod arr_length in
        Array.get arr rotated_index 

    let set (arr: 'a array) (index: int) (value: 'a) : unit =
        let arr_length = Array.length arr in
        let rotated_index = index mod arr_length in
        Array.set arr rotated_index value

    let rec get_slice (arr: 'a array) ~start ~count : 'a list =
        if count = 0 
        then []
        else 
            (get arr start) :: (get_slice arr ~start:(start + 1) ~count:(count - 1))

    let rec set_slice (arr: 'array) ~start (values: 'a list) : unit = 
        match values with 
        | [] -> ()
        | next::rest -> 
                set arr start next;
                set_slice arr ~start:(start + 1) rest

    let reverse_slice (arr: 'a array) ~start ~count : unit =
        let slice = get_slice arr ~start ~count in
        set_slice arr ~start (List.rev slice)
end

(* Debugging helper *)
let print_arr_with_range arr idx target_idx = 
    Array.to_list arr 
    |> List.mapi (fun i x -> 
        let prefix = if i = idx then "(" else "" in (* Bracket the range we're going to flip *)
        let suffix = if i = (target_idx - 1) then ")" else "" in
        Printf.sprintf "%s%d%s" prefix x suffix
    )
    |> String.concat " "
    |> Printf.printf "[%s]\n\n\n"

(* This method optionally accepts mutable refs for index/skip-size so that it's 
 * easier to keep track of "current progress" for part 2 *)
let process_arr arr ?current_index:(current_index=ref 0) ?skip_size:(skip_size=ref 0) lengths =
    List.iter (fun jump_length ->
        (* let target = (!current_index + jump_length) mod 256 in *)
        (* Printf.printf "Current Index: %d Length: %d " !current_index jump_length; *)
        (* print_arr_with_range arr !current_index target; *)
        CircularArray.reverse_slice arr ~start:(!current_index) ~count:jump_length;
        current_index := ((!current_index + jump_length + !skip_size) mod 256);
        incr skip_size
    ) lengths;
    (* print_arr_with_range arr !current_index (-1); *)
    arr

let day10a (input: string) : int = 
    let naive_numeric_lengths =
        input
        |> AdventStdLib.Strings.split_on ","
        |> List.map int_of_string
    in
    let part_a_array = Array.of_list (AdventStdLib.Ints.range 0 255) in
    let day1a_result_array = process_arr part_a_array naive_numeric_lengths in
    match Array.to_list day1a_result_array with 
        | a::b::_ ->  a * b
        | _ -> failwith "Something went wrong pattern-matching the processed array!"

let print_list stringifier l =
    print_string "[";
    l
    |> List.map stringifier
    |> String.concat "; "
    |> print_string;
    print_endline "]"

let day10b (input: string) : string =
    let ascii_encoded_lengths = 
        let suffix = [17;31;73;47;23] in
        AdventStdLib.Strings.explode_str input
        |> List.map (AdventStdLib.Strings.to_char)
        |> List.map (int_of_char)
        |> (fun prefix -> List.append prefix suffix)
    in
    let round_count = 64 in
    let current_index = ref 0 in
    let skip_size = ref 0 in
    let arr = Array.of_list (AdventStdLib.Ints.range 0 255) in
    (* Run 64 rounds of array-processing *)
    for _ = 1 to round_count do
        ignore @@ process_arr arr ~current_index ~skip_size ascii_encoded_lengths;
    done;
    (* Post-processing on array contents *)
    arr
    |> Array.to_list
    |> AdventStdLib.Lists.chunk ~size:16    (* Chunk it up into 16-int chunks *)
    |> List.map (List.fold_left (lxor) 0)   (* XOR all the elements of the chunk together *)
    |> List.map (Printf.sprintf "%02x")     (* Convert to 2-digit hex-string representation with leading 0 *)
    |> String.concat ""                     (* And concatenate into a single string *)

let read_input () : string =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.hd

let () =
    let input = read_input() in
    day10a input |> Printf.printf "Day 10A: %d\n";
    day10b input |> Printf.printf "Day 10B: %s\n"
