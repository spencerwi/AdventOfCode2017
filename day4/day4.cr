class Day4
    @input : Array(Array(String))

    def initialize(filename : String)
        @input = File.read_lines(filename).map{|line| line.split}
    end

    # How many lines have no duplicate words?
    def part_a
        count_valid_lines {|line| line.uniq == line} # if the deduplicated set of words is the same as the original set, there are no dupes.
    end

    # How many lines have no anagram-duplicate words?
    def part_b
        count_valid_lines do |line|
            if line.uniq == line # first a straight-duplicate check to save some operations
                # Next check for anagrams. If one word is an anagram of another, 
                #  that means it contains *exactly* the same letters. So if we
                #  rearrange those letters alphabetically in both words, we 
                #  the "letter-sorted" versions of those words should be 
                #  identical, so we can then just dupe-check the same way as before.
                letter_sorted_words = line.map {|word| word.chars.sort}
                letter_sorted_words.uniq == letter_sorted_words
            else
                false
            end
        end
    end

    private def count_valid_lines(&line_validator : Array(String) -> Bool) : Int32
        @input.select {|line| yield line}.size
    end
end

day4 = Day4.new("input.txt")
puts "Day 4A: #{day4.part_a}"
puts "Day 4B: #{day4.part_b}"
