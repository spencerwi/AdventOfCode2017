(* This stuff up here is how you get a map/set in OCaml. It's kinda annoying *)
module IntModule = struct 
    type t = int
    let compare = compare
end
module IntMap = Map.Make(IntModule)
module IntSet = Set.Make(IntModule)

(* A type to represent "1 <-> 2,3,4" as 1 connected to 2, 3, and 4 *)
type program = {
    id: int;
    connected_to: int list
}
(* A friendlier type alias for a map from a program id to its connected programs *)
type program_bank = int list IntMap.t

let parse_program (s: string) : program = 
    (* Split "1 <-> 2, 3, 4" into "1" and "2, 3, 4" *)
    match AdventStdLib.Strings.split_on " <-> " s with
    | [prog;connected_progs_str] -> 
      (* then split "2, 3, 4" into "2", "3", and "4" *)
      let connected_progs = 
          connected_progs_str 
          |> AdventStdLib.Strings.split_on ", "
          |> List.map int_of_string 
      in
      (* and build a "program" record out of it *)
      {id = int_of_string prog; connected_to = connected_progs}
    | _ -> failwith @@ "Invalid connection string: " ^ s

(* Take all our programs and build a "connection" mapping *)
let build_program_bank (all_programs: program list) : program_bank = 
    List.fold_left (fun map_so_far program ->
        IntMap.add program.id program.connected_to map_so_far
    ) IntMap.empty all_programs

(* This is glorified tree walking, with a circular tree *)
let rec find_connected_programs (prog_bank: program_bank) (seen_list: IntSet.t) (group_leader: int) : IntSet.t =
    if IntSet.mem group_leader seen_list then
        (* If we've already seen this connection, then stop -- we're done *)
        seen_list
    else
        (* Otherwise, get all direct connections for the current "leader" *)
        let direct_connections = IntMap.find group_leader prog_bank in
        (* Update our "seen list" to include the current "leader" *)
        let updated_seen_list = IntSet.add group_leader seen_list in
        (* And recursively walk the "tree" *)
        List.fold_left (find_connected_programs prog_bank) updated_seen_list direct_connections


let read_input () : program_bank =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.map parse_program
    |> build_program_bank

let day12a input =
    find_connected_programs input IntSet.empty 0
    |> IntSet.cardinal

let day12b input =
    (* Make a list of all unique program names *)
    let all_unique_programs = 
        input
        |> IntMap.bindings
        |> List.map fst
        |> IntSet.of_list
    in
    (* So that we can keep a running list of which programs we haven't visited *)
    let unvisited = ref all_unique_programs in
    (* And a counter of how many complete program groups we've seen *)
    let group_count = ref 0 in
    (* Loop until we've visited all programs *)
    while not (IntSet.equal !unvisited IntSet.empty) do
        (* Grab a program from our unvisited-programs list *)
        let next = IntSet.choose !unvisited in
        (* Find its program group *)
        let connected_to_next = find_connected_programs input IntSet.empty next in
        (* Remove all programs in that group from our "unvisited" list *)
        unvisited := IntSet.diff !unvisited connected_to_next;
        (* And increment our group counter *)
        incr group_count
    done;
    !group_count

let () =
    let input = read_input() in
    input |> day12a |> Printf.printf "Day 12A: %d\n";
    input |> day12b |> Printf.printf "Day 12B: %d"
