(* OCaml fanfare for generating a module for maps from int to something *)
module IntMap = Map.Make(struct 
    type t = int
    let compare = compare
end)

(* Scanner definitions and operations *)
module Scanner = struct
    type t = {
        range: int 
    }
    let make range = {range}

    let is_safe_at_time time scanner = 
        (* A scanner with range 2 can be in positions 0 or 1 *)
        let max_position = scanner.range - 1 in
        let scanner_is_back_at_top = 
            (* If a scanner depth is 3, its max position is 2. 
             * It then travels 0 -> 1 -> 2 -> 1 -> 0, and repeat.
             * Consequently, this means it takes 4 steps to return to the top.
             * 4 is 2 * 2 -- and this holds true for any given depth. *)
            time mod (2 * max_position) = 0
        in
        not scanner_is_back_at_top
end

(* A bank of scanners is a map from int (depth) to scanner *)
type scanner_bank = Scanner.t IntMap.t

(* Parse the input lines into a scanner bank *)
let parse_scanner_bank (input: string list) : scanner_bank =
    let parse_line line = 
        line                                    (* "4: 5" *)
        |> AdventStdLib.Strings.split_on ": "   (* ["4"; "5"] *)
        |> List.map int_of_string               (* [4; 5] *)
        |> (function                            (* (4, scanner with range 5) or else fail *)
            | [depth;range] -> (depth, Scanner.make range)  
            | _ -> failwith @@ Printf.sprintf "Invalid scanner specification: %s" line 
        )
    in
    input
    |> List.map parse_line
    |> List.fold_left (fun bank (depth, scanner) -> IntMap.add depth scanner bank) IntMap.empty

let read_input() : scanner_bank =
    AdventStdLib.read_lines_from_file "input.txt"
    |> parse_scanner_bank

(* A helper function for running a "trip" with given delay *)
let run_trip ?delay:(delay=0) scanner_bank = 
    (* We want to track the trip severity (for part A) and how many times the packet was caught (for part B) *)
    let severity = ref 0 in
    let times_packet_was_caught = ref 0 in

    IntMap.iter (fun depth scanner ->
        (* For each scanner, check if the scanner is safe at the time the packet
         * arrives -- that is, the scanner depth plus any delay. If it's not 
         * safe, track that we hit the scanner and update the severity. *)
        if not (Scanner.is_safe_at_time (delay + depth) scanner) then begin
            incr times_packet_was_caught;
            severity := !severity + (depth * scanner.range)
        end
    ) scanner_bank;
    (!times_packet_was_caught, !severity)

let day13a scanner_bank = 
    run_trip scanner_bank

let day13b scanner_bank =
    (* Keep trying various delay timings until one's safe (i.e. 0 "catches") *)
    let safe_delay = ref None in
    let currently_checked_delay = ref 1 in (* We know 0 delay isn't safe *)
    while !safe_delay = None do
        let (times_packet_was_caught, _) = 
            run_trip ~delay:(!currently_checked_delay) scanner_bank 
        in
        if times_packet_was_caught = 0 then 
            safe_delay := Some !currently_checked_delay;

        incr currently_checked_delay
    done;
    AdventStdLib.Options.unwrap_exn !safe_delay

let () = 
    let input = read_input() in
    input |> day13a |> snd |> Printf.printf "Day 13A: %d\n";
    input |> day13b |> Printf.printf "Day 13B: %d\n"
