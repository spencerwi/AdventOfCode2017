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
