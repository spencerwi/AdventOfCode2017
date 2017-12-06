module StringSet = Set.Make(struct 
    type t = string
    let compare = compare
end);;

let read_input () =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.map (AdventStdLib.String.split_on " +")

let count_valid_lines (validator: string list -> bool) (lines: string list list) : int =
    lines 
    |> Stream.of_list
    |> AdventStdLib.Stream.filter validator
    |> AdventStdLib.Stream.length

let line_has_dupe_word words_in_line = 
    let unique_words_in_line = List.sort_uniq compare words_in_line in
    (List.compare_lengths unique_words_in_line words_in_line) = -1 

let day4a lines =
    count_valid_lines (fun line -> not @@ line_has_dupe_word line) lines

let day4b lines =
    let line_has_anagram_word words_in_line =
        let sort_letters_in_word word =
            word 
            |> AdventStdLib.String.explode_str 
            |> List.sort compare
            |> String.concat ""
        in
        let words_in_line_rearranged_by_letter = List.map sort_letters_in_word words_in_line in
        line_has_dupe_word words_in_line_rearranged_by_letter
    in count_valid_lines (fun line -> not @@ line_has_anagram_word line) lines



let () =
    let input = read_input() in
    let print_result = Printf.printf "Day 4%s: %d\n" in
    day4a input |> print_result "A" ;
    day4b input |> print_result "B"

