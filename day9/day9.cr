class Day9
    @input : Array(Char)

    def initialize(filename : String)
        @input = File.read(filename).chars
    end

    def part_a : Int32
        score = 0
        group_value = 0
        index = 0
        is_garbage = false
        while index < @input.size # Manual iteration so we can skip chars as needed
            next_char = @input[index]
            if is_garbage
                case next_char
                when '!' then index += 1
                when '>' then is_garbage = false
                end
            else
                case next_char
                when '{' then group_value += 1
                when '<' then is_garbage = true
                when '}' 
                    score += group_value
                    group_value -= 1
                end
            end
            index += 1
        end
        score
    end

    def part_b
        garbage_char_count = 0
        index = 0
        is_garbage = false
        while index < @input.size # Again, manual iteration for skipping purposes
            next_char = @input[index]
            if is_garbage
                case next_char
                when '!' then index += 1
                when '>' then is_garbage = false
                else garbage_char_count += 1
                end
            else 
                if next_char == '<' 
                    is_garbage = true
                end
            end
            index += 1
        end
        garbage_char_count += 1
    end
end

day9 = Day9.new "input.txt"
puts "Day 9A: #{day9.part_a}"
puts "Day 9B: #{day9.part_b}"
