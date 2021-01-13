module CI2

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

struct WithNode <: AE
    the_symbol::Symbol
    binding_expr::AE
    body::AE
end

struct SymbolNode <: AE
    the_symbol::Symbol
end

abstract type RetVal
end

struct MtEnvironment <: Environment
end

struct ConcreteEnvironment <: Environment
    the_symbol::Symbol
    value::RetVal
    parent::Environment
end

#
# =====================================
#

function parse(tok::Number)
    return NumNode(tok)
end

function parse(tok::Symbol)
    return SymbolNode(tok)
end


function parse(toks::Array{Any})
    # in class example
    if length(toks) == 3
        return parse3(toks)
    elseif length(toks) == 4
        return parse4(toks)
    else
        throw(LispError("Cardinality error."))
    end
end

function parse3(toks::Array{Any})
    if toks[1] == :+
        return PlusNode(parse(toks[2]), parse(toks[3]))
    elseif toks[1] == :-
        return MinusNode(parse(toks[2]), parse(toks[3]))
    else
        throw(LispError("Invalid operator"))
    end
end

function parse4(toks::Array{Any})
    if toks[1] == :if0
        return If0Node(parse(toks[2]), parse(toks[3]))
    elseif toks[1] == :with
        return WithNode(toks[2])
    else
        throw(LispError("Invalid operator."))
        # etc etc
    end
end

#
# =========================================================
#

function calc(ast::NumNode, env::Environment)
    return ast.the_number
end

function calc(ast::PlusNode, env::Environment)
    return calc(ast.lhs, env) + calc(ast.rhs, env)
end

function calc(ast::MinusNode, env::Environment)
    return calc(ast.lhs, env) - calc(ast.rhs, env)
end

function calc(ast::If0Node, env::Environment)
    if calc(ast.cond) == 0
        return calc(ast.zerobranch, env)
    else
        return calc(ast.nonzerobranch, env)
    end
end

function calc(ast::SymbolNode, env::MtEnvironment)
    throw(LispError("undefined variable"))
end

function calc(ast::SymbolNode, env::ConcreteEnvironment)
    if ast.the_symbol == env.the_symbol
        return env.value
    else
        return calc(ast, env.parent)
    end
end

function calc(ast::WithNode, env::Environment)
    ConcreteEnvironment(ast.the_symbol, calc(ast.binding_expr, env), env)
    return calc(ast.body, new_env)
end

#
# =============================================
#

function interp(cs::AbstractString)
    tokens = Lexer.lex(cs)
    ast = parse(tokens)
    return calc(ast, MtEnvironment())
end

end #module
