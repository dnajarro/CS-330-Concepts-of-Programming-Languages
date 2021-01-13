module RudInt

using Error
using Lexer

export interpf

abstract type AE
end

struct NumNode <: AE
	n::Real
end

struct BinopNode <: AE
	op::Function
	lhs::AE
	rhs::AE
end

struct UnopNode <: AE
	op::Function
	arg::AE
end

#
# ==============================================
#

function calc(ast::NumNode)
    return ast.n
end

function calc(ast::BinopNode)
	if ast.op == +
		return calc(ast.lhs) + calc(ast.rhs)
	elseif ast.op == -
		return calc(ast.lhs) - calc(ast.rhs)
	elseif ast.op == *
		return calc(ast.lhs) * calc(ast.rhs)
	elseif (ast.op == /) || (ast.op == %)
		rhs = calc(ast.rhs)
		if rhs == 0
			throw(LispError("Invalid input: division by zero."))
		elseif ast.op == /
			return calc(ast.lhs) / calc(ast.rhs)
		else
			return mod(calc(ast.lhs), calc(ast.rhs))
		end
	end
end

function calc(ast::UnopNode)
	if ast.op == -
		return -1 * calc(ast.arg)
	elseif ast.op == collatz
		return collatz(calc(ast.arg))
	end
end

function parse(tok::Real)
    return NumNode(tok)
end

function parse(toks::Array{Any})
    # in class example
	if length(toks) < 2
		throw(LispError("Too few arguments."))
	elseif length(toks) == 2
		return parse2(toks)
    elseif length(toks) == 3
        return parse3(toks)
    else
		throw(LispError("Cardinality error."))
	end
end

function parse2(toks::Array{Any})
	dict = Dict(
	  :+ => +,
	  :- => -,
	  :* => *,
	  :/ => /,
	  :mod => %,
	  :collatz => collatz
	)
    if toks[1] == :-
		return UnopNode(dict[toks[1]], parse(toks[2]))
    elseif toks[1] == :collatz
		return UnopNode(dict[toks[1]], parse(toks[2]))
	else
		throw(LispError("Cardinality error."))
    end
end

function parse3(toks::Array{Any})
	# change to contains function
	dict = Dict(
	  :+ => +,
	  :- => -,
	  :* => *,
	  :/ => /,
	  :mod => %,
	  :collatz => collatz
	)
	if isBinop(toks[1])
		return BinopNode(dict[toks[1]], parse(toks[2]), parse(toks[3]))
	elseif toks[1] == :collatz
		throw(LispError("Too many arguments. Collatz takes two arguments."))
	else
		throw(LispError("First argument must be a valid function."))
	end
end

function isZeroNumNode(ast::NumNode)
	if ast.n == 0
		return true
	end
	return false
end

function isBinop(sym::Any)
	if sym == :+
		return true
	elseif sym == :-
		return true
	elseif sym == :*
		return true
	elseif sym == :/
		return true
	elseif sym == :mod
		return true
	else
		return false
	end
end

function collatz( n::Real )
	return collatz_helper( n, 0 )
end

function collatz_helper( n::Real, num_iters::Int )
	if n <= 0
		throw(LispError("Invalid input. Collatz function needs a positive number."))
	end
	if n == 1
		return num_iters
	end
	if mod(n, 2) == 0
		return collatz_helper(n / 2, num_iters + 1)
	else
		return collatz_helper(3 * n + 1, num_iters + 1)
	end
end

function interpf(cs::AbstractString)
    tokens = Lexer.lex(cs)
    ast = parse(tokens)
    return calc(ast)
end

end #module
