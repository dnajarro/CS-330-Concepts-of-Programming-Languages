module CI0_inclass

# in your REPL, also add using Revise to see changes
using Error
using Lexer

export interp

abstract type AE
end

struct NumNode <: AE
    the_number::Number
end

struct PlusNode <: AE
    lhs::AE
    rhs::AE
end

struct MinusNode <: AE
    lhs::AE
    rhs::AE
end

struct BinopNode <: AE
	op::Function
	lhs::AE
	rhs::AE
end

dict = Dict(
    :+ => +,
    :- => -
)

function parse(tok::Number)
    return NumNode(tok)
end

function calc(ast::NumNode)
    return ast.the_number
end

function calc(ast::PlusNode)
    return calc(ast.lhs) + calc(ast.rhs)
end

function calc(ast::MinusNode)
    return calc(ast.lhs) - calc(ast.rhs)
end

function calc(ast::BinopNode)
	return calc(ast.lhs) + calc(ast.rhs)
end

function parse(toks::Array{Any})
    if length(toks) != 3
        throw(LispError("Cardinality error."))
    end

	# return BinopNode(dict[toks[1]], parse(toks[2]), parse(toks[3]))

    if toks[1] == :+
        return PlusNode(parse(toks[2]), parse(toks[3]))
    elseif toks[1] == :-
        return MinusNode(parse(toks[2]), parse(toks[3]))
    else
        throw(LispError("Invalid operator. You done messed up!"))
    end
end

function interp(cs::AbstractString)
    tokens = Lexer.lex(cs)
    ast = parse(tokens)
    return calc(ast)
end

end #module
