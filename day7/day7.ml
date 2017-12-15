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
            |> List.find_opt AdventStdLib.Option.is_some 
            |> AdventStdLib.Option.flatten

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
                let children_names = AdventStdLib.Option.get_or_default maybe_children_names ~default_value:[] in
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
    let open Tree in
    root.name

let day7b root =
    let open Tree in
    failwith "TODO: Part B"


let () =
    let input = read_input() in
    let print_result = Printf.printf "Day 7%s: %s\n" in
    let root = get_root_node input in
    root |> day7a |> print_result "A"
