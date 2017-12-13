module StringMap = Map.Make(String)

(* A module to encapsulate instruction parsing and evaluation *)
module Instruction = struct
    type register_bank = int StringMap.t

    (* A module to encapsulate predicate parsing and evaluation *)
    module Predicate = struct
        type comparison_operator = LessThan | LessThanOrEqual | Equal | NotEqual | GreaterThanOrEqual | GreaterThan

        (* Given: 
               foo >= 5
            lh = "foo", comparison_operator = GreaterThanOrEqual, rh = 5
         *)
        type t = {
            lh: string;
            comparison_operator: comparison_operator;
            rh: int
        }
        let evaluate (registers : register_bank) predicate : bool =
            let comparator_fn = match predicate.comparison_operator with 
                | LessThan              -> (<) 
                | LessThanOrEqual       -> (<=)
                | Equal                 -> (==)
                | NotEqual              -> (!=)
                | GreaterThanOrEqual    -> (>=)
                | GreaterThan           -> (>)
            in
            let lh_value = 
                StringMap.find_opt predicate.lh registers
                |> AdventStdLib.Option.get_or_default ~default_value:0
            in
            comparator_fn lh_value predicate.rh
    end

    type operation = Increment | Decrement

    (* Given:
     *    foo incr 10 if bar >= 4
     *  register = "foo", operation = Increment, amount = 10, and predicate = a predicate for "bar >= 4"
     *)
    type t = {
        register: string;
        operation: operation;
        amount: int;
        predicate: Predicate.t
    }

    let execute (registers: register_bank) instruction =
        (* First check the predicate to see if we should update our register bank *)
        let should_modify = Predicate.evaluate registers instruction.predicate in
        if should_modify then
            (* If we should update it, evaluate the expression *)
            let operator_fn = match instruction.operation with
                | Increment -> (+)
                | Decrement -> (-)
            in
            let existing_value = 
                StringMap.find_opt instruction.register registers
                |> AdventStdLib.Option.get_or_default ~default_value:0
            in
            let new_value = operator_fn existing_value instruction.amount in
            (* And return an updated register bank *)
            StringMap.add instruction.register new_value registers
        else
            (* If we should not update, return the unmodified register bank *)
            registers

    module Parsing = struct 
        module RegexpParser = MParser.MakeRegexp(MParser_RE.Regexp)
        type 'a parser = ('a, unit) MParser.t

        (* Parses out a register name *)
        let parse_register : string parser =
            let open MParser in
            many_chars letter <?> "Register name"

        (* parses out a predicate from a string the form 
         *   if RHS OPERATOR LHS 
         *  example:
         *   if foo >= 0
         *)
        let parse_predicate : Predicate.t parser =
            let open MParser in
            let p_lh = (string "if" >> spaces >> parse_register << spaces) <?> "Predicate LHS" in
            let p_operator = choice [
                (string "<=" >> return Predicate.LessThanOrEqual);
                (string ">=" >> return Predicate.GreaterThanOrEqual);
                (string "!=" >> return Predicate.NotEqual);
                (string "==" >> return Predicate.Equal);

                (* Put these two last, so that we don't misread "<=" as "<" and an erroneous "=" *)
                (string "<"  >> return Predicate.LessThan);
                (string ">"  >> return Predicate.GreaterThan)
            ] <?> "Predicate Operator" in 
            let p_rh = (spaces >> RegexpParser.Tokens.integer) <?> "Predicate RHS" in
            let build_record lh comparison_operator rh =
                let open Predicate in
                { lh; comparison_operator; rh }
            in
            MParser.pipe3 p_lh p_operator p_rh build_record

        let parse_instruction : t parser =
            let open MParser in
            let p_operation = choice [
                (string "inc" >> return Increment);
                (string "dec" >> return Decrement)
            ] <?> "Register Operation" in
            let p_amount = (spaces >> RegexpParser.Tokens.integer) <?> "Register modifier amount" in
            let build_record register operation amount predicate =
                { register; operation; amount; predicate }
            in
            MParser.pipe4 parse_register (spaces >> p_operation) (spaces >> p_amount) (spaces >> parse_predicate) build_record
    end
end

let read_input () : Instruction.t list =
    AdventStdLib.read_lines_from_file "input.txt"
    |> List.map (fun line -> MParser.parse_string Instruction.Parsing.parse_instruction line ())
    |> List.map (function
        | MParser.Success a -> a
        | MParser.Failed (msg, e) -> failwith msg
    )

let get_highest_value (registers: Instruction.register_bank) : int option =
    StringMap.bindings registers
    |> List.map (fun (key, value) -> value)
    |> List.sort compare
    |> AdventStdLib.List.last


let day8 (input : Instruction.t list) : (int * int) =
    let highest_ever_value = ref (-99999999) in
    let initial_state : Instruction.register_bank = StringMap.empty in
    let execute_and_update_highest registers_state instruction =
        begin match get_highest_value registers_state with 
            | Some x when x > !highest_ever_value -> highest_ever_value := x
            | _ -> ()
        end;
        Instruction.execute registers_state instruction
    in
    let final_registers = 
        List.fold_left execute_and_update_highest initial_state input
    in
    let final_highest_value = AdventStdLib.Option.unwrap_exn @@ get_highest_value final_registers in
    (final_highest_value, !highest_ever_value)

let () =
    let input = read_input() in
    let print_result = Printf.printf "Day 8%s: %d\n" in
    let (final_highest_value, highest_ever_value) = day8 input in
    print_result "A" final_highest_value;
    print_result "B" highest_ever_value
