open AdventStdLib

type registers = int CharMap.t 
module Duet = struct
    module Instruction = struct
        type operand = Register of char | Value of int [@@deriving show]
        type t = 
          | Snd of operand
          | Rcv of operand 
          | Set of char * operand
          | Add of char * operand
          | Mul of char * operand
          | Mod of char * operand
          | Jgz of operand * operand
        [@@deriving show]

        let instruction_parser : (t, unit) MParser.t = 
            let open MParser in 
            let module RegexParser = MParser.MakeRegexp(MParser_RE.Regexp) in
            let integer = RegexParser.Tokens.integer in  (* Alias for convenience *)
            let operand_parser = choice [
                letter  |>> (fun c -> Register c);
                integer |>> (fun i -> Value i)
            ] in
            let binary_op_parser prefix_str operand1 constructor =
                string prefix_str >> (pipe2 operand1 (spaces >> operand_parser)) constructor 
            in
            choice [ 
                string "snd " >> operand_parser |>> (fun x -> Snd x);
                string "rcv " >> operand_parser |>> (fun x -> Rcv x);
                binary_op_parser "set " letter (fun x y -> Set (x, y));
                binary_op_parser "add " letter (fun x y -> Add (x, y));
                binary_op_parser "mul " letter (fun x y -> Mul (x, y));
                binary_op_parser "mul " letter (fun x y -> Mul (x, y));
                binary_op_parser "mod " letter (fun x y -> Mod (x, y));
                binary_op_parser "jgz " operand_parser (fun x y -> Jgz (x,y))
            ]
    end

    type t = {
        mutable registers: registers;
        mutable last_sound_played: int option;
        mutable program_counter: int;
        mutable rcv_history: int list;
        program: Instruction.t array (* Arrays are better for arbitrary-index access *)
    }  
    let make program =
        {registers = CharMap.empty; last_sound_played = None; program_counter = 0; rcv_history = []; program} 

    (* How to evaluate operands *)
    let eval_operand registers = function
        | Instruction.Value i -> i
        | Instruction.Register c -> CharMap.find_opt c registers |> Options.get_or_default ~default_value:0

    (* Some convenience aliases for register operations *)
    let set reg value registers = CharMap.add reg value registers
    let add reg increment registers =
        CharMap.update reg (function 
            | Some x -> Some (x + increment)
            | None -> Some increment
        ) registers
    let mul reg multiplicand registers =
        CharMap.update reg (function 
            | Some x -> Some (x * multiplicand)
            | None -> Some 0
        ) registers
    let mod_ reg divisor registers =
        CharMap.update reg (function 
            | Some x -> Some (x mod divisor)
            | None -> Some 0
        ) registers

    (* Convenience alias for "Rcv" *)
    let rcv a duet = 
        match duet.last_sound_played with
        | Some x -> duet.rcv_history <- (x :: duet.rcv_history)
        | None -> ()


    let execute duet = 
        let program_size = Array.length duet.program in
        let has_recovered_sound = ref false in
        while (duet.program_counter < program_size) && not (!has_recovered_sound) do
            let next_instruction = duet.program.(duet.program_counter) in
            begin match next_instruction with 
                (* Sound operations *)
                | Snd a -> duet.last_sound_played <- Some (eval_operand duet.registers a)
                | Rcv a when (eval_operand duet.registers a) = 0 -> () (* no-op on "rcv 0" *)
                | Rcv a -> begin 
                        rcv (eval_operand duet.registers a) duet;
                        has_recovered_sound := true
                    end

                (* Register operations *)
                | Set (reg, value)          -> duet.registers <- set reg (eval_operand duet.registers value) duet.registers
                | Add (reg, increment)      -> duet.registers <- add reg (eval_operand duet.registers increment) duet.registers
                | Mul (reg, multiplicand)   -> duet.registers <- mul reg (eval_operand duet.registers multiplicand) duet.registers
                | Mod (reg, divisor)        -> duet.registers <- mod_ reg (eval_operand duet.registers divisor) duet.registers

                (* Jumping *)
                | Jgz (value, jump_amount) when (eval_operand duet.registers value) <= 0 -> () (* no-op if value <= 0 *)
                | Jgz (value, jump_amount)  -> duet.program_counter <- duet.program_counter + ((eval_operand duet.registers jump_amount) - 1) (* subtract one to offset the "natural" stepping *)
            end;
            duet.program_counter <- duet.program_counter + 1
        done
end

let read_input() : Duet.t =
    read_lines_from_file "input.txt"
    |> List.map (fun line -> MParser.parse_string Duet.Instruction.instruction_parser line ())
    |> List.map (function 
        | MParser.Success instruction -> instruction
        | MParser.Failed (msg, err) -> failwith msg
    )
    |> Array.of_list
    |> Duet.make

let day18a () =
    let duet = read_input() in
    Duet.execute duet;
    List.hd duet.rcv_history

let () =
    day18a() |> Printf.printf "Day 18a: %d\n"
