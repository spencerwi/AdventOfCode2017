class Day5

    @input : Array(Int32)

    def initialize(input_filename : String)
        @input = File.read_lines(input_filename)
            .map {|line| line.to_i}
    end

    def part_a
        process_instructions {|jump_length| jump_length + 1}
    end

    def part_b
        process_instructions do |jump_length| 
            if jump_length >= 3
                jump_length - 1
            else
                jump_length + 1
            end
        end
    end

    # Processes the instruction list, updating each "jumped-from" value using
    #  the incrementer block provided, and returns how many jumps it takes to
    #  "escape" the maze
    private def process_instructions(&incrementer : Int32 -> Int32) : Int32 
        safe_working_copy = @input.clone # so that part a doesn't clobber part b
        maze_length = safe_working_copy.size
        current_index = 0
        jump_count = 0
        while current_index < maze_length && current_index >= 0
            jump_amount = safe_working_copy[current_index]
            safe_working_copy[current_index] = yield jump_amount # to the incrementer function
            current_index += jump_amount
            jump_count += 1
        end
        jump_count
    end
end

day5 = Day5.new "input.txt"
puts "Day 5A: #{day5.part_a}"
puts "Day 5B: #{day5.part_b}"
