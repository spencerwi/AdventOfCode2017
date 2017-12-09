let read_input () =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.hd
    |> int_of_string


let generate_distance_from_corner_map_by_side_length side_length =
    let farthest_distance_from_center_of_side = side_length / 2 in
    (* This will give us a list like, for example, [2, 1, 0, 1, 2] which is the 
     *  map of distances from the center for each arm. If we take the distance 
     *  from the corner to the center, and subtract each "slot" in the map from 
     *  it (and then absolute-value it, because distances aren't negative), we 
     *  get a map of distances for each spot from the corners, which we can use 
     *  later in part A to determine manhattan distance from the center of the 
     *  spiral. *)
    let side_distance_from_center_map = 
        AdventStdLib.Int.range_inclusive 
            (farthest_distance_from_center_of_side * -1) 
            farthest_distance_from_center_of_side
        |> List.map abs
    in
    side_distance_from_center_map
    |> List.map (fun x -> farthest_distance_from_center_of_side - x) 
    |> List.map abs


let day3a (input: int) : int =
    (* The bottom-right "corners" of each square "layer" are 1^2, 3^2, 5^2, etc  *)
    let odd_numbers = Stream.from (fun x -> Some ((x * 2) + 1)) in
    (* Every corner in a ring (not just bottom-right corners) is the same 
        manhattan distance from the center of the spiral:
           sqrt_bottom_right_corner - 1
        So we count how far from a corner our spot is. The further from a 
        corner, the closer it is to the center of the side, meaning fewer steps 
        needed to get back to the center of the spiral.
    *)
    let sqrt_bottom_right_corner = 
        AdventStdLib.Stream.find_first (fun odd_num -> (odd_num * odd_num) > input) odd_numbers
    in
    let corner_manhattan_distance = (sqrt_bottom_right_corner - 1) in
    let bottom_right_corner_number = (sqrt_bottom_right_corner * sqrt_bottom_right_corner) in
    (* The length of each layer's sides, in steps, is the square-root of the bottom-right corner, minus 1 *)
    let length_of_each_side = sqrt_bottom_right_corner - 1 in
    (* How many steps do we have to walk backwards? *)
    let steps_back_away_from_corner = bottom_right_corner_number - input in
    (* Where on each side does that leave us? *)
    let position_on_side = (steps_back_away_from_corner mod length_of_each_side) in
    (* Generate a "map" that tells us how many steps each spot on a side is from a corner,
        e.g. [0, 1, 2, 1, 0] *)
    let corner_distance_map = 
        generate_distance_from_corner_map_by_side_length length_of_each_side 
    in
    let distance_from_corner = List.nth corner_distance_map position_on_side in
    corner_manhattan_distance - distance_from_corner

let day3b input =
    (* Credit to the Online Encyclopedia of Integer Sequences, which is 
        apparently a thing that exists. This sequence is apparently already known.  *)
    let sequence = [
        1; 1; 2; 4; 5; 10; 11; 23; 25; 26; 54; 57; 59; 122; 133; 142; 147; 304; 330;
        351; 362; 747; 806; 880; 931; 957; 1968; 2105; 2275; 2391; 2450; 5022; 5336;
        5733; 6155; 6444; 6591; 13486; 14267; 15252; 16295; 17008; 17370; 35487; 37402;
        39835; 42452; 45220; 47108; 48065; 98098; 103128; 109476; 116247; 123363;
        128204; 130654; 266330; 279138; 295229; 312453; 330785; 349975; 363010; 369601;
        752688; 787032; 830037; 875851; 924406; 975079; 1009457; 1026827; 2089141;
        2179400; 2292124; 2411813; 2539320; 2674100; 2814493; 2909666; 2957731; 6013560;
        6262851; 6573553; 6902404; 7251490; 7619304; 8001525; 8260383; 8391037;
        17048404; 17724526; 18565223; 19452043; 20390510; 21383723; 22427493; 23510079;
        24242690; 24612291; 49977270; 51886591; 54256348; 56749268; 59379562; 62154898;
        65063840; 68075203; 70111487; 71138314; 144365769; 149661137; 156221802;
        163105139; 170348396; 177973629; 186001542; 194399801; 203081691; 208949088;
        211906819; 429827198; 445061340; 463911304; 483650112; 504377559; 526150757;
        549023076; 572904288; 597557233; 614208653; 622599690; 1262247784; 1305411751;
        1358749904; 1414491696; 1472899472; 1534125748; 1598327474; 1665648769;
        1735829031; 1808194091; 1857049072; 1881661363; 3813299996; 3939776148;
        4095896357; 4258788564; 4429173742; 4607457470; 4794055770; 4989349711;
        5192600241; 5401925245; 5543175046; 5614313360; 11372992489; 11738157709;
        (* and so on *)
    ]
    in
    match List.find_opt (fun x -> x > input) sequence with 
    | Some a -> a 
    | None -> failwith "Outside known sequence. Yes, this is cheating a bit."

    
let () =
    let input = read_input() in
    let print_result = Printf.printf "Day 3%s: %d\n" in
    day3a input |> print_result "A" ;
    day3b input |> print_result "B" 
