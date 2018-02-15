module Generator = struct
    type t = 
      { mutable previous: int; (* mutability, in this case, is more efficient than immutable return-updated-copy *)
        factor: int }

    let make (seed: int) (factor: int) : t =
        {previous = seed; factor}

    let get_next (generator: t) : int =
        let next_value = (generator.previous * generator.factor) mod 2147483647 in
        generator.previous <- next_value;
        next_value

    let rec get_next_divisible_by (divisor: int) (generator: t) : int =
        let next = get_next generator in
        if (next mod divisor) = 0 
        then next
        else get_next_divisible_by divisor generator
end

let same_last_16_bits a b =
    (* Use a bitmask of 0xFFFF -- all-ones in the lowest 16 -- to "zero out" all
     * bits higher than the lowest 16, so we can compare just those. *)
    let masked_a = a land 0xFFFF in
    let masked_b = b land 0xFFFF in
    masked_a = masked_b

let run_generators (generator_a, divisor_a) (generator_b, divisor_b) pairs_to_generate =
    let generator_a = Generator.make 618 16807 in
    let generator_b = Generator.make 814 48271 in
    let match_count = ref 0 in
    for i = 0 to pairs_to_generate do
        let next_a = Generator.get_next_divisible_by divisor_a generator_a in
        let next_b = Generator.get_next_divisible_by divisor_b generator_b in
        if same_last_16_bits next_a next_b then incr match_count
    done;
   !match_count


let day15a () =
    let generator_a = Generator.make 618 16807 in
    let generator_b = Generator.make 814 48271 in
    run_generators (generator_a, 1) (generator_b, 1) 40_000_000

 let day15b () =
    let generator_a = Generator.make 618 16807 in
    let generator_b = Generator.make 814 48271 in
    run_generators (generator_a, 4) (generator_b, 8) 5_000_000

let () =
    day15a() |> Printf.printf "Day 15A: %d\n";
    day15b() |> Printf.printf "Day 15B: %d\n"

