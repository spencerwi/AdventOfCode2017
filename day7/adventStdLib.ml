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
end

module Ints = struct 
    let bigger a b =
        if a > b then a else b
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

    let count_element_occurrences (l: 'a list) : ('a, int) Hashtbl.t =
        let counts = Hashtbl.create (List.length l) in
        l |> List.iter (fun next -> 
            let updated_count = 
                match Hashtbl.find_opt counts next with
                | Some cnt -> cnt + 1
                | None -> 1
            in
            Hashtbl.replace counts next updated_count;
        );
        counts

    let to_string (l: string list) : string =
        String.concat "; " l
        |> Printf.sprintf "[%s]"
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

    let flatten = function 
        | Some (Some x) -> Some x
        | _ -> None 

    let map f = function
        | Some x -> Some (f x)
        | None -> None

    (* Also known as "bind" *)
    let flat_map f opt =
        opt |> map f |> flatten
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

module Hashtbls = struct
    let to_assoc_list tbl =
        Hashtbl.fold (fun a b l -> (a,b) :: l) tbl []
        |> List.rev
end

module Debug = struct
    let tap f x =
        f x;
        x

end
