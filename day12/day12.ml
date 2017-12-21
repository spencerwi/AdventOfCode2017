module IntModule = struct 
    type t = int
    let compare = compare
end
module IntMap = Map.Make(IntModule)
module IntSet = Set.Make(IntModule)
type program = {
    id: int;
    connected_to: int list
}
type program_bank = int list IntMap.t

let parse_program (s: string) : program = 
    match AdventStdLib.Strings.split_on " <-> " s with
    | [prog;connected_progs_str] -> 
      let connected_progs = 
          connected_progs_str 
          |> AdventStdLib.Strings.split_on ", "
          |> List.map int_of_string 
      in
      {id = int_of_string prog; connected_to = connected_progs}
    | _ -> failwith @@ "Invalid connection string: " ^ s

let build_program_bank (all_programs: program list) : program_bank = 
    List.fold_left (fun map_so_far program ->
        IntMap.add program.id program.connected_to map_so_far
    ) IntMap.empty all_programs

let rec find_connected_programs (prog_bank: program_bank) (seen_list: IntSet.t) (group_leader: int) : IntSet.t =
    let direct_connections = 
        IntMap.find group_leader prog_bank
        |> List.filter (fun connected_prog -> connected_prog <> group_leader)
    in
    let unvisited_next_steps = 
        direct_connections
        |> List.filter (fun connection -> not @@ IntSet.mem connection seen_list)
    in
    let updated_seen_list = 
        List.fold_left (fun set child -> IntSet.add child set) seen_list direct_connections 
        |> IntSet.add group_leader
    in
    if unvisited_next_steps = [] 
    then seen_list
    else 
        unvisited_next_steps
        |> List.map (find_connected_programs prog_bank updated_seen_list)
        |> List.fold_left IntSet.union updated_seen_list

let read_input () : program_bank =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.map parse_program
    |> build_program_bank

let day12a input =
    find_connected_programs input IntSet.empty 0
    |> IntSet.cardinal

let day12b input =
    let all_unique_programs = 
        input
        |> IntMap.bindings
        |> List.map fst
        |> IntSet.of_list
    in
    let unvisited = ref all_unique_programs in
    let groups = ref [] in
    while not (IntSet.equal !unvisited IntSet.empty) do
        let next = IntSet.choose !unvisited in
        let connected_to_next = find_connected_programs input IntSet.empty next in
        unvisited := IntSet.diff !unvisited connected_to_next;
        groups := connected_to_next :: !groups
    done;
    List.length !groups

let () =
    let input = read_input() in
    input |> day12a |> Printf.printf "Day 12A: %d";
    input |> day12b |> Printf.printf "Day 12B: %d"
