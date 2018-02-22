alias Allocation = Array(Int32)

class Day6

    @input : Allocation
    @loop_state : Allocation | Nil # Nil until part_a detects the "loop state"

    def initialize(input_filename : String) 
        @input = File.read(input_filename).split.map{|x| x.to_i}
        @loop_state = nil
    end

    def part_a
        seen_allocations = Set(Allocation).new()
        current_allocation = @input
        count_until_cycle = 0
        while !seen_allocations.includes?(current_allocation)
            seen_allocations.add(current_allocation)
            count_until_cycle += 1
            current_allocation = reallocate(current_allocation)
        end
        @loop_state = current_allocation
        count_until_cycle
    end

    def part_b
        if @loop_state == nil
            raise "Should have done part a first!"
        else
            current_allocation = reallocate(@loop_state.not_nil!)
            cycle_length = 1
            while current_allocation != @loop_state.not_nil!
                current_allocation = reallocate(current_allocation)
                cycle_length += 1
            end
            cycle_length
        end
    end

    private def reallocate(current_allocation : Allocation) : Allocation
        largest = current_allocation.max.not_nil!
        current_index = current_allocation.index(largest).not_nil!
        new_allocation = current_allocation.clone
        new_allocation[current_index] = 0
        largest.times do
            current_index = (current_index + 1) % new_allocation.size
            new_allocation[current_index] += 1
        end
        new_allocation
    end

end

day6 = Day6.new("input.txt")
puts "Day 6A: #{day6.part_a}"
puts "Day 6B: #{day6.part_b}"
