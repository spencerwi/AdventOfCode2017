module HexGrid = struct
    type coordinates = int * int * int

    let start_position = (0,0,0)

    let apply_coordinate_diff (x,y,z) (xdiff,ydiff,zdiff) = (x+xdiff, y+ydiff, z+zdiff)

    (* Reference: https://www.redblobgames.com/grids/hexagons/#coordinates-cube *)
    let move_direction current_pos = function 
        | "n"  -> apply_coordinate_diff current_pos ( 0,  1, -1)
        | "ne" -> apply_coordinate_diff current_pos ( 1,  0, -1)
        | "se" -> apply_coordinate_diff current_pos ( 1, -1,  0)
        | "s"  -> apply_coordinate_diff current_pos ( 0, -1,  1)
        | "sw" -> apply_coordinate_diff current_pos (-1,  0,  1)
        | "nw" -> apply_coordinate_diff current_pos (-1,  1,  0)
        | _ -> failwith "Invalid direction!"

	(* Reference: https://www.redblobgames.com/grids/hexagons/#distances-cube *)
    let distance (x1,y1,z1) (x2,y2,z2) : int = 
        AdventStdLib.Lists.max [x2 - x1; y2 - y1; z2 - z1]
        |> AdventStdLib.Options.unwrap_exn
end

let read_input() : string list =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.hd
    |> AdventStdLib.Strings.split_on ","

let day11 input : int * int =
	let farthest_distance = ref 0 in
    let end_position = 
        List.fold_left (fun current_pos next_step -> 
			let current_distance = HexGrid.distance current_pos HexGrid.start_position in
			if current_distance > !farthest_distance then 
				farthest_distance := current_distance;

			HexGrid.move_direction current_pos next_step
		) HexGrid.start_position input
    in
	let end_distance = HexGrid.distance end_position HexGrid.start_position  in
	(end_distance, !farthest_distance)

let () =
    let input = read_input() in
    let print_result = Printf.printf "Day 11%s: %d\n" in
	let end_distance, farthest_distance = day11 input in
    print_result "A" end_distance;
	print_result "B" farthest_distance
    
