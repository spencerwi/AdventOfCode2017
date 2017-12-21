module Tree = struct 
    type t = {
        name: string;
        mutable weight: int;
        (* Useful for identifying the root: the root has no parent *)
        mutable parent: t option;
        mutable children: t list;
        (* Temporary storage for names of child trees needing to be attached to 
         * this tree *)
        children_names: string list
    } 

    (* Tree-building utils *)

    let add_child (parent: t) (child: t) =
        parent.children <- (child :: parent.children);
        child.parent <- Some parent

    let update_weight t weight =
        t.weight <- weight

    let make_empty_tree name weight children_names : t =
        {name; weight; parent = None; children = []; children_names}

    let rec find_child_by_name (name: string) (tree: t) : t option =
        if tree.name = name 
        then Some tree
        else
            tree.children
            |> List.map (find_child_by_name name)
            |> List.find_opt AdventStdLib.Options.is_some 
            |> AdventStdLib.Options.flatten


    (* Tree-balancing utils *)
     
    let rec get_total_weight t =
        if t.children = [] 
        then t.weight
        else 
            let children_total_weight = 
                t.children
                |> List.map get_total_weight
                |> List.fold_left (+) 0
            in
            t.weight + children_total_weight

    let rec is_balanced t = 
        let distinct_weights = 
            t.children 
            |> List.map get_total_weight
            |> List.sort_uniq compare
        in
        (List.length distinct_weights) = 1

    let get_common_child_weight t =
        if t.children = []
        then None
        else 
            t.children
            |> List.map get_total_weight
            |> AdventStdLib.Lists.count_element_occurrences
            |> AdventStdLib.Hashtbls.to_assoc_list
            |> List.find_opt (fun (weight, count) -> count > 1)
            |> AdventStdLib.Options.map fst

    let find_local_outlier t =
        if t.children = [] 
        then None
        else
            let common_weight = 
                get_common_child_weight t
                |> AdventStdLib.Options.unwrap_exn 
            in
            t.children
            |> List.find_opt (fun child -> (get_total_weight child) != common_weight)

    (* Debug utils *)

    let to_short_string t = 
        Printf.sprintf "%s (%d)" t.name (get_total_weight t)

    let rec to_string ?shift_amount:(shift_amount=0) ?depth:(depth=None) (t: t) = 
        let shift_prefix =
            String.init shift_amount (fun _ -> '\t') 
        in
        let parent_name = 
            t.parent 
            |> AdventStdLib.Options.map (fun parent -> parent.name) 
            |> AdventStdLib.Options.get_or_default ~default_value:"None"
        in
        let children_str = match depth with
        | None | Some 0 -> "<omitted>"
        | Some d ->
            t.children
            |> List.map (to_string ~shift_amount:(shift_amount + 1) ~depth:(Some (d - 1)))
            |> String.concat (";\n" ^ shift_prefix ^ "\t")
            |> Printf.sprintf "[%s]"
        in
        Printf.sprintf "{
%s    name = %s,
%s    weight = %d,
%s    total_weight = %d,
%s    parent = %s,
%s    children = %s
%s}" shift_prefix t.name shift_prefix t.weight shift_prefix (get_total_weight t) shift_prefix parent_name shift_prefix children_str shift_prefix


    module Parsing = struct 
        module RegexpParser = MParser.MakeRegexp(MParser_RE.Regexp)
        type 'a parser = ('a, unit) MParser.t

        let tree_parser : t parser = 
            let open MParser in
            let parse_name = (many_chars letter) in
            let parse_weight = spaces >> RegexpParser.Tokens.parens RegexpParser.Tokens.integer in
            let parse_children_names = spaces >> (string "->") >> spaces >> (MParser.sep_by1 parse_name (char ',' >> spaces)) in
            let parse_rest_of_line = (MParser.option parse_children_names) << eof in
            pipe3 parse_name parse_weight parse_rest_of_line (fun name weight maybe_children_names ->
                let children_names = AdventStdLib.Options.get_or_default maybe_children_names ~default_value:[] in
                make_empty_tree name weight children_names
            )

        let parse_tree_from_string (line_num: int) (str: string) : t =
            match MParser.parse_string tree_parser str () with
                | MParser.Success t -> t
                | MParser.Failed (msg, e) -> failwith (Printf.sprintf "Line %d: %s" line_num msg)
    end
end


let read_input () : Tree.t list =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.mapi Tree.Parsing.parse_tree_from_string

let get_root_node (input: Tree.t list) : Tree.t =
    let open Tree in
    (* For each tree in the list, grab out the names of child nodes needing to
     * be attached to this child, then find those in the list and attach them. *)
    let attach_children t =
        (* If there are no children for this node, we're done *)
        if (t.children_names != []) 
        then
            (* Otherwise, for each child_name in children_names, find that tree
             * in our larger list of all parsed-out trees, and attach it to the
             * parent in question *)
            List.iter (fun child_name ->
                let child = List.find (fun x -> x.name = child_name) input in
                Tree.add_child t child
            ) t.children_names
    in List.iter attach_children input;
    (* The root node is the node that has no parent *)
    List.find (fun t -> t.parent = None) input 

let day7a root =
    (* Simple: just spit out the root node's name *)
    let open Tree in
    root.name

let day7b root =
    let open Tree in
    (* Search for the outlier-by-total-weight where all of its children are 
     * either balanced or nonexistent -- this is the one node whose weight we 
     * need to change. *)
    let current_node = ref root in
    while (not @@ is_balanced !current_node) && (!current_node.children != []) do
        match find_local_outlier !current_node with 
        | Some outlier -> current_node := outlier
        | _ -> ();
    done;
    let outlier = !current_node in
    (* Having found it, look at its siblings to determine what its total weight 
     * *should* be, then take the difference between the target total weight and
     * the current total weight, and apply that difference to the node's own 
     * "local" weight. *)
    let outlier_parent = AdventStdLib.Options.unwrap_exn outlier.parent in
    let target_total_weight = AdventStdLib.Options.unwrap_exn (get_common_child_weight outlier_parent) in
    let outlier_total_weight = get_total_weight outlier in
    let difference = target_total_weight - outlier_total_weight in
    let target_local_weight = outlier.weight + difference in
    let decision_string = Printf.sprintf "%s would need to weigh %d instead of %d" 
        (to_short_string outlier) 
        target_local_weight 
        outlier.weight 
    in
    (* Then print our decision (and the full tree, for verification *)
    Printf.sprintf "%s\nFull outlier tree (with parent):\n%s" 
        decision_string 
        (to_string ~depth:(Some 1) outlier_parent)
        

let () =
    let input = read_input() in
    let print_result = Printf.printf "Day 7%s: %s\n" in
    let root = get_root_node input in
    root |> day7a |> print_result "A";
    root |> day7b |> print_result "B";


