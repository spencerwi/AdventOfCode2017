class Day2

    @input : Array(Array(Int32))

    def initialize(input_filename : String)
        @input = File.read_lines(input_filename)
            .map {|line| line.split }
            .map {|line| line.map {|num| num.to_i}}
    end

    def part_a 
        checksum_lines do |line|
            smallest, largest = line.minmax
            largest - smallest
        end
    end

    def part_b
        checksum_lines do |line|
            divisible_pair = line.permutations(2)
                .find {|pair| (pair[0] % pair[1] == 0) || (pair[1] % pair[0] == 0)}
            if divisible_pair 
                smaller, larger = divisible_pair.minmax
                larger / smaller
            else
                raise "No pairs were divisible; this shouldn't happen"
            end
        end
    end

    private def checksum_lines(&checksum_func: Array(Int32) -> Int32)
        @input.map {|line| yield line }.sum
    end
end

day2 = Day2.new "input.txt"
puts "Day 2A: #{day2.part_a}"
puts "Day 2B: #{day2.part_b}"
