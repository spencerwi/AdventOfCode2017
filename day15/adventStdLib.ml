let read_lines_from_file (filename: string) : string list =
    let lines = ref [] in
    let chan = open_in filename in
    try
        while true; do
            lines := (input_line chan) :: !lines
        done;
        !lines
    with End_of_file -> 
        close_in chan;
        List.rev !lines

module Strings = struct 
    let rec explode_str = function
        | "" -> []
        | s  -> (String.sub s 0 1) :: explode_str (String.sub s 1 (String.length s - 1))

    let split_on delim s = 
        Str.split (Str.regexp delim) s

    let of_char c = 
        String.make 1 c

    let to_char s =
        if String.length s = 1 
        then String.get s 0
        else failwith "Cannot to_char a string of length > or < 1"
end

module Ints = struct 
    let bigger a b =
        if a > b then a else b

    let rec range start stop =
        if start > stop 
        then []
        else 
            let next = start + 1 in
            start :: (range next stop)

    (* Taken from RosettaCode *)
    let to_binary_string (n: int) : string =
        if n < 0 then failwith "Cannot binary-ize a negative number"
        else if n = 0 then "0" 
        else
            let rec aux acc d = 
                if d = 0 then acc 
                else aux (string_of_int (d land 1) :: acc) (d lsr 1)
            in
            String.concat "" (aux [] n)
end


module Lists = struct
    let split_at idx l =
        let rec split' i acc = function
            | [] -> (List.rev acc, [])
            | x::rest as l -> 
                    if i = 0 
                    then (List.rev acc, l)
                    else split' (i-1) (x::acc) rest 
        in
        split' idx [] l

    let rec all_but idx = function
        | [] -> []
        | _::rest when idx = 0 -> rest
        | x::rest -> x :: (all_but (idx - 1) rest)


    let rec last = function
        | [] -> None
        | a::[] -> Some a
        | _::rest -> last rest

    let rec flat_map f l = List.flatten @@ List.map f l

    let chunk ~size (l: 'a list) =
        let rec chunk_ (chunks: 'a list list) (current_chunk: 'a list) (counter: int) = function
            | [] -> 
                    if current_chunk = [] 
                    then List.rev chunks
                    else List.rev ((List.rev current_chunk) :: chunks)
            | next::rest -> begin
                    let new_current_chunk = (next :: current_chunk) in
                    if counter = size then
                        let current_chunk_in_correct_order = List.rev new_current_chunk in
                        let new_chunks = (current_chunk_in_correct_order :: chunks) in
                        chunk_ new_chunks [] 1 rest
                    else
                        chunk_ chunks new_current_chunk (counter + 1) rest
            end
        in
        chunk_ [] [] 1 l

    let sum_int ?initial_value:(initial_value=0) l =
        List.fold_left (+) initial_value l

end

module Options = struct
    let is_some = function
        | Some _ -> true
        | None -> false

    let is_none x = not @@ is_some x

    let unwrap_exn = function
        | Some x -> x
        | None -> failwith "Tried to unwrap none"

    let get_or_default ~default_value = function
        | Some x -> x
        | None -> default_value
end

module Streams = struct 
    let rec take_while (predicate: 'a -> bool) (s: 'a Stream.t) =
        let next = Stream.next s in
        if (predicate next) 
        then next :: take_while predicate s
        else []

    let rec find_first (predicate: 'a -> bool) (s: 'a Stream.t) =
        let next = Stream.next s in
        if (predicate next) 
        then next 
        else find_first predicate s

    let length stream =
        let result = ref 0 in
        Stream.iter (fun _ -> result := (!result + 1)) stream;
        !result

    let to_list stream =
        let l = ref [] in
        let add_to_list x = 
            l := x :: !l
        in
        Stream.iter add_to_list stream;
        List.rev !l

    (* taken from ocaml.org *)
        
    let map f stream =
        let rec next i =
            try Some (f (Stream.next stream))
            with Stream.Failure -> None in
        Stream.from next

    let filter p stream =
        let rec next i =
            try
                let value = Stream.next stream in
                if p value then Some value else next i
            with Stream.Failure -> None in
        Stream.from next

    let fold f stream init =
        let result = ref init in
        Stream.iter (fun x -> result := f x !result) stream;
    !result;; 

end

module Arrays = struct
    let max_index arr = 
        let max_value = ref (-9999999999) in
        let max_index = ref (-1) in
        Array.iteri (fun idx curr -> 
            if curr > !max_value 
            then (
                max_value := curr;
                max_index := idx;
            )
        ) arr;
        !max_index

    let modify_element arr f idx =
        let previous_value = Array.get arr idx in
        Array.set arr idx (f previous_value)
end

module Debug = struct
    let tap (f: 'a -> unit) (x: 'a) : 'a = 
        f x;
        x

end

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

(* from Day 10 *)
module KnotHashes = struct
    type t = int list (* of length 16 *)

    let compute_hash (input: string) : t =
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
        in
        let ascii_encoded_lengths = 
            let suffix = [17;31;73;47;23] in
            Strings.explode_str input
            |> List.map (Strings.to_char)
            |> List.map (int_of_char)
            |> (fun prefix -> List.append prefix suffix)
        in
        let round_count = 64 in
        let current_index = ref 0 in
        let skip_size = ref 0 in
        let arr = Array.of_list (Ints.range 0 255) in
        (* Run 64 rounds of array-processing *)
        for _ = 1 to round_count do
            ignore @@ process_arr arr ~current_index ~skip_size ascii_encoded_lengths;
        done;
        (* Post-processing on array contents *)
        arr
        |> Array.to_list
        |> Lists.chunk ~size:16    (* Chunk it up into 16-int chunks *)
        |> List.map (List.fold_left (lxor) 0)   (* XOR all the elements of the chunk together *)

    let to_hex_string (hash: t) : string =
        hash
        |> List.map (Printf.sprintf "%02x")     (* Convert to 2-digit hex-string representation with leading 0 *)
        |> String.concat ""                     (* And concatenate into a single string *)

    let to_binary_string (hash: t) : string =
        hash 
        |> List.map (Ints.to_binary_string)
        |> String.concat ""
end
