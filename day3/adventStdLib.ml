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

module String = struct 
    let rec explode_str = function
        | "" -> []
        | s  -> (String.sub s 0 1) :: explode_str (String.sub s 1 (String.length s - 1))

    let split_on delim s = 
        Str.split (Str.regexp delim) s
end


module List = struct
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
end

module Option = struct
    let is_some = function
        | Some _ -> true
        | None -> false

    let is_none x = not @@ is_some x

    let unwrap_exn = function
        | Some x -> x
        | None -> failwith "Tried to unwrap none"
end

module Stream = struct 
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
end

module Int = struct
    let rec range_inclusive start stop =
        if start > stop 
        then []
        else 
            let next = start + 1 in
            start :: (range_inclusive next stop)
end
