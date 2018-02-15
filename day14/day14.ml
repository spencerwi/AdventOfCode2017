let input = "vbqugkhl" (* Hard-coding this time to avoid having to read a file *)

let hashed_rows : AdventStdLib.KnotHashes.t list = 
    Stream.from (function | x when x < 128 -> Some x | _ -> None)
    |> AdventStdLib.Streams.map (Printf.sprintf "%s-%d" input)
    |> AdventStdLib.Streams.map AdventStdLib.KnotHashes.compute_hash
    |> AdventStdLib.Streams.to_list

let day14a () =
    (* Using Kernighan's algorithm *)
    let rec count_set_bits_in_int n =
        if n = 0 then 0
        else 
            let new_n = (n land (n - 1)) in
            1 + (count_set_bits_in_int new_n)
    in
    hashed_rows
    |> List.map (fun row -> 
        List.map count_set_bits_in_int row
        |> AdventStdLib.Lists.sum_int
    )
    |> AdventStdLib.Lists.sum_int

let () =
    day14a() |> Printf.printf "Day 14A: %d\n"
