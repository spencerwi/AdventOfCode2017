class Day1

    @input : Array(Int32)

    def initialize(input_filename : String)
        @input = File.read(input_filename)
            .chars
            .reject {|c| c.whitespace?}
            .map {|c| c.to_i}
    end

    def part_a()
        rotated_input = @input.rotate()
        total = 0
        rotated_input.each_cons(2) do  |pair|
            total += pair[0] if pair[0] == pair[1] 
        end
        total
    end

    def part_b()
        total = 0
        input_length = @input.size
        @input.each_with_index do |x, idx| 
            halfway_around_idx = (idx + (input_length / 2)) % input_length
            total += x if x == @input[halfway_around_idx]
        end
        total
    end
end

day1 = Day1.new "input.txt"
puts "Day 1A: #{day1.part_a()}"
puts "Day 1B: #{day1.part_b()}"
