class Tree
    property name : String
    property weight : Int32
    property parent : Tree | Nil
    property children : Array(Tree)
    property children_names : Array(String) # temporary storage for child names so we can match them up

    def initialize(name : String, weight : Int32, children_names : Array(String))
        @name = name
        @weight = weight
        @parent = nil
        @children = [] of Tree
        @children_names = children_names
    end


    def add_child(child : Tree)
        child.parent = self
        @children << child
    end

    def find_child_by_name(name : String) : Tree | Nil
        if @name == name
            self
        else
            @children.map {|child| child.find_child_by_name(name)}
                .find {|x| x != nil}
        end
    end

    def get_total_weight : Int32
        if @children.empty?
            @weight
        else
            children_total_weight : Int32 = @children.map{|child| child.get_total_weight.as(Int32) }.sum(0)
            @weight + children_total_weight
        end
    end

    def is_balanced : Bool
        group_weights_by_count.size == 1
    end

    def get_common_child_weight : Int32 | Nil
        group_weights_by_count[@children.size - 1]
    end

    def find_local_outlier : Tree | Nil
        common_weight = get_common_child_weight
        @children.find {|child| child.get_total_weight != common_weight}
    end

    private def group_weights_by_count : Hash(Int32, Int32) # Count -> Weight
        @children.map {|child| child.get_total_weight}
            .group_by {|x| x}
            .map {|weight, instances| [instances.size, weight]}
            .to_h
    end
end

class Day7
    @input : Array(Tree)

    def initialize(input_filename : String)
        @input = 
            File.read_lines(input_filename)
                .map do |line|
                    root_and_children_names = line.split(" -> ")
                    children_names = 
                        if (root_and_children_names.size > 1)
                            root_and_children_names[1].split(", ")
                        else
                            [] of String
                        end
                    root_name, root_weight_str = root_and_children_names[0].split
                    root_weight = root_weight_str.delete('(').delete(')').to_i
                    Tree.new(root_name.not_nil!, root_weight, children_names)
                end
        @input.each {|tree| fixup_children(tree)}
    end

    def part_a() : String
        find_root_node().name
    end

    def part_b() : String
        # Search for the outlier-by-total-weight where all of its children are
        #  balanced or nonexistent.
        current_node : Tree = find_root_node()
        while !current_node.is_balanced && !current_node.children.empty?
            outlier = current_node.find_local_outlier
            current_node = outlier.not_nil!
        end

        outlier = current_node.not_nil!
        # Having found the outlier, look at its siblings to determine 
        #  its "target total weight"
        outlier_parent : Tree = outlier.parent.not_nil!
        target_total_weight = outlier_parent.get_common_child_weight
        outlier_total_weight = outlier.get_total_weight

        # "Apply" the difference between its target total weight and its current
        #  total weight to its own weight
        difference = target_total_weight - outlier_total_weight
        target_local_weight = outlier.weight + difference
        "#{outlier.name} would need to weight #{target_local_weight} instead of #{target_total_weight}"
    end

    private def fixup_children(tree : Tree) # Using child names, match up and attach children
        tree.children_names.each do |child_name|
            child = @input.find{|t| t.name == child_name}.not_nil!
            tree.add_child(child)
        end
    end
    private def find_root_node() : Tree # there *is* a root, hence non-nilable
        @input.find {|node| node.parent == nil}.not_nil! 
    end
end

day7 = Day7.new("input.txt")
puts "Day 7A: #{day7.part_a}"
puts "Day 7B: #{day7.part_b}"
