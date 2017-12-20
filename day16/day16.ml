module DanceLine = struct 
    type t = string array
    let to_string l = 
        l |> Array.to_list |> String.concat ""

    let make_line () = 
        ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p"]
        |> Array.of_list

    (* "Low-level" operations *)
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

    module DanceMoves = struct
        type t =
            Spin of int
          | Exchange of int * int
          | Swap of string * string

        let spin spin_count line =
            if spin_count > 0 
            then set_slice line ~start:spin_count (Array.to_list line)

        let exchange pos1 pos2 line =
            let dancer1 = line.(pos1) in
            let dancer2 = line.(pos2) in
            line.(pos1) <- dancer2;
            line.(pos2) <- dancer1

        let swap dancer1 dancer2 line =
            let pos1 = AdventStdLib.Arrays.find_index_exn line dancer1 in
            let pos2 = AdventStdLib.Arrays.find_index_exn line dancer2 in
            line.(pos1) <- dancer2;
            line.(pos2) <- dancer1

        let eval dance_line = function
            | Spin amount             -> spin amount dance_line
            | Exchange (pos1, pos2)   -> exchange pos1 pos2 dance_line
            | Swap (dancer1, dancer2) -> swap dancer1 dancer2 dance_line

        module Parsing = struct
            type 'a parser = ('a, unit) MParser.t
            module RegexParser = MParser.MakeRegexp(MParser_RE.Regexp)

            let move_parser : t parser =
                let open MParser in
                let spin_parser = 
                    char 's' >> RegexParser.Tokens.integer 
                    |>> (fun spin_amount -> Spin spin_amount)
                in 
                let exchange_parser =
                    let pos1_p = char 'x' >> RegexParser.Tokens.integer in
                    let pos2_p = char '/' >> RegexParser.Tokens.integer in
                    pipe2 pos1_p pos2_p (fun pos1 pos2 -> Exchange (pos1, pos2))
                in
                let swap_parser = 
                    let dancer_name_p = (any_char |>> AdventStdLib.Strings.of_char) in
                    let dancer1_p = char 'p' >> dancer_name_p in
                    let dancer2_p = char '/' >> dancer_name_p in
                    pipe2 dancer1_p dancer2_p (fun dancer1 dancer2 -> Swap (dancer1, dancer2))
                in
                choice [ spin_parser; exchange_parser; swap_parser ]

            let movelist_parser : t list parser =
                let open MParser in
                sep_by1 move_parser (char ',')

            let parse_moves (input: string) : t list =
                match MParser.parse_string movelist_parser input () with
                | MParser.Success movelist -> movelist
                | MParser.Failed (msg, e) -> failwith ("Failed to parse movelist: " ^ msg)
        end
    end
end
let read_input () : DanceLine.DanceMoves.t list =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.hd
    |> DanceLine.DanceMoves.Parsing.parse_moves

let () =
    let input = read_input() in
    let dance_line = DanceLine.make_line() in

    (* Convenience method for dancing and to_string-ing *)
    let perform_dance_and_get_result () =
        List.iter (DanceLine.DanceMoves.eval dance_line) input;
        DanceLine.to_string dance_line
    in

    (* Part A: what's the result after one dance? *)
    let part_a_result = perform_dance_and_get_result() in
    Printf.printf "Day 16A: %s\n" part_a_result;

    (* Now for part B, we need to "run" the dance 1 billion times (including the
     *  first time). Except running it 1 billion times would take too long. 
     *  Luckily, it turns out that after a certain point, there's a cycle. So 
     *  let's keep track of how many times we run the dance before we observe a 
     *  cycle, then we just need the result of dancing 
     *      (1billion mod cycle_length) 
     *  times *)
    let dance_lines_seen = ref [part_a_result] in
    let cycle_length = ref 1 in
    let cycle_found = ref false in
    while not !cycle_found do
        let result = perform_dance_and_get_result() in
        cycle_found := List.mem result !dance_lines_seen;
        if not !cycle_found then (
            incr cycle_length;
            dance_lines_seen := result :: !dance_lines_seen
        ) else
            (* since we've been prepending, our list is in reverse order *)
            dance_lines_seen := List.rev !dance_lines_seen 
    done;
    let target_result_index = (1_000_000_000 mod !cycle_length) - 1 in
    (* The "- 1" above is because we need a 0-based index, not a 1-based one *)
    let final_state = List.nth !dance_lines_seen target_result_index in
    Printf.printf "Day 16B: %s\n" final_state
