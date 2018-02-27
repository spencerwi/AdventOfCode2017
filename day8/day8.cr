alias Registers = Hash(String, Int32)

enum TermType
    Value,
    Variable
end
class Term
    @type : TermType
    getter value : Int32?
    getter varname : String?   

    protected def initialize(type : TermType, value : Int32?, varname : String?)
        @type = type
        @value = value
        @varname = varname
    end

    def self.new_val(value : Int32) : Term
        Term.new(TermType::Value, value, nil)
    end
    
    def self.new_var(varname : String) : Term
        Term.new(TermType::Variable, nil, varname)
    end

    def self.parse(str : String) : Term
        begin # try to parse as int
            int_val = str.to_i
            Term.new_val int_val.not_nil!
        rescue ArgumentError # not numeric, try varname
            Term.new_var str
        end
    end

    def eval(registers : Registers)  : Int32
        case @type
        when TermType::Value then @value.not_nil!
        when TermType::Variable then registers.fetch(@varname.not_nil!, 0)
        else raise "Invalid term!"
        end
    end
end

enum ComparisonOperator
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
    GreaterThanOrEqual,
    GreaterThan
end
class Predicate
    @lhs : Term
    @comparison_op : ComparisonOperator
    @rhs : Term

    def initialize(lhs : Term, comparison_op : ComparisonOperator, rhs : Term)
        @lhs = lhs
        @comparison_op = comparison_op
        @rhs = rhs
    end

    def self.parse(str : String) : Predicate
        lhs_str, op_str, rhs_str = str.split
        lhs = Term.parse(lhs_str)
        rhs = Term.parse(rhs_str)
        op = 
            case op_str 
            when "<"  then ComparisonOperator::LessThan
            when "<=" then ComparisonOperator::LessThanOrEqual
            when "!=" then ComparisonOperator::NotEqual
            when "==" then ComparisonOperator::Equal
            when ">=" then ComparisonOperator::GreaterThanOrEqual
            when ">"  then ComparisonOperator::GreaterThan
            else raise "Invalid comparison operator : #{op_str}"
            end
        Predicate.new(lhs, op, rhs)
    end

    def eval(registers : Registers) : Bool
        lhs_val = @lhs.eval(registers)
        rhs_val = @rhs.eval(registers)
        case @comparison_op
        when .lessThan?             then @lhs < @rhs
        when .lessThanOrEqual?      then @lhs <= @rhs
        when .equal?                then @lhs == @rhs
        when .notEqual?             then @lhs != @rhs
        when .greaterThanOrEqual?   then @lhs >= @rhs
        when .greaterThan?          then @lhs > @rhs
        else raise "Invalid predicate!"
        end
    end
end

enum Operation
    Increment,
    Decrement
end
class Instruction
    @register : String
    @operation : Operation
    @amount : Int32
    @predicate : Predicate?

    def initialize(register : String, operation : Operation, amount : Int32, predicate : Predicate?)
        @register = register
        @operation = operation
        @amount = amount
        @predicate = predicate
    end

    def self.parse(str : String) : Instruction
        words = str.split
        register = words[0]
        operation = 
            case words[1].downcase
            when "inc" then Operation::Increment
            when "dec" then Operation::Decrement
            else raise "Invalid operation: #{words[1].downcase}"
            end
        amount = words[2].to_i
        predicate = 
            if words.size > 3 
                Predicate.parse(words[4..6].join(" "))
            else
                nil
            end
        Instruction.new(register, operation, amount, predicate)
    end

    def eval(registers : Registers) 
        if @predicate != nil && @predicate.not_nil!.eval(registers)
            old_value = registers[@register]
            new_value = 
                case @operation
                when .increment? then old_value + @amount
                when .decrement? then old_value - @amount
                else raise "Invalid operation: #{@operation}"
                end
            registers[@register] = new_value
        end
    end
end

class Day8
    @input : Array(Instruction)

    def initialize(input_filename : String)
        @input = File.read_lines(input_filename)
                     .map {|line| Instruction.parse(line)}
    end

    def solve
        highest_ever_value = Int32::MIN
        registers = Hash(String, Int32).new(0)
        @input.each do |instruction|
            instruction.eval(registers)
            if !registers.empty?
                current_max_value = registers.values.max.not_nil!
                highest_ever_value = [current_max_value, highest_ever_value].max.not_nil!
            end
        end
        final_highest_value = registers.values.max.not_nil!
        {highest_ever_value, final_highest_value}
    end
end

day8 = Day8.new "input.txt"
highest_ever_value, final_highest_value = day8.solve
puts "Day 8A: #{final_highest_value}"
puts "Day 8A: #{highest_ever_value}"
