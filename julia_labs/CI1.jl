module CI1

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

struct If0Node <: AE
    cond::AE
    zerobranch::AE
    nonzerobranch::AE
end



#
# =====================================
#

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

function parse(toks::Array{Any})
    # in class example
    if length(toks) == 3
        return parse3(toks)
    elseif length(toks) == 4
        return parse4(toks)
    else throw(LispError("Cardinality error."))
    end
end

function parse3(toks::Array{Any})
    if toks[1] == :+
        return PlusNode(parse(toks[2]), parse(toks[3]))
        # etc etc
    end
end

function parse4(toks::Array{Any})
    if toks[1] == :if0
        return If0Node(parse(toks[2]), parse(toks[3]))
    elseif toks[1] == :+
        throw(LispError("Cardinality error."))
        # etc etc
    end
end

function interp(cs::AbstractString)
    tokens = Lexer.lex(cs)
    ast = parse(tokens)
    return calc(ast)
end

end #module
