#TODO: check the arity in the calc functions, not the parse
#TODO: check for type errors
#TODO: check for syntax errors in the parse functions

module RudInt

using Error
using Lexer

export interpf

abstract type AE
end

# <AE> ::= <number>
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

# <AE> ::= (if0 <AE> <AE> <AE>)
struct If0Node <: AE
    cond::AE
    zerobranch::AE
    nzerobranch::AE
end

# <AE> ::= (with <id> <AE> <AE>)
# (with ((some list of ids bound to some value, i.e., x = 5)) (some function/action that you want to do with those variables you just defined, i.e., (+ x 3)))
struct WithNode <: AE
 sym::Symbol
 binding_expr::AE
 body::AE
end

# <AE> ::= <id>
struct VarRefNode <: AE
  sym::Symbol
end

# <AE> ::= (lambda <id> <AE>)
struct FunDefNode <: AE
  formal::Symbol
  fun_body::AE
end

# <AE> ::= (<AE> <AE>)
struct FuncAppNode <: AE
    fun_expr::AE
    arg_expr::AE
end

#
# ==============================================
#

abstract type RetVal
end

abstract type Environment
end

struct NumVal <: RetVal
    n::Real
end

struct ClosureVal <: RetVal
  formal::Symbol
  body::AE
  env::Environment
end

#
# ===============================================
#

struct EmptyEnv <: Environment
end

struct ExtendedEnv <: Environment
  sym::Symbol
  val::RetVal
  parent::Environment
end

#
# ===============================================
#

function parse(tok::Real)
    return NumNode(tok)
end

function parse(tok::Symbol)
    return VarRefNode(tok)
end

function parse(toks::Array{Any})
	if toks[1] == :+
		return BinopNode(dict[toks[1]], parse(toks[2]), parse(toks[3]))
	elseif toks[1] == :-
		if length(toks) > 2
			return BinopNode(dict[toks[1]], parse(toks[2]), parse(toks[3]))
		else
			return UnopNode(dict[toks[1]], parse(toks[2]))
	elseif toks[1] == :/
		return BinopNode(dict[toks[1]], parse(toks[2]), parse(toks[3]))
	elseif toks[1] == :*
		return BinopNode(dict[toks[1]], parse(toks[2]), parse(toks[3]))
	elseif toks[1] == :mod
		return BinopNode(dict[toks[1]], parse(toks[2]), parse(toks[3]))
	elseif toks[1] == :collatz
		return UnopNode(dict[toks[1]], parse(toks[2]))
	elseif toks[1] == :if0
		return If0Node(parse(toks[2]), parse(toks[3]), parse(toks[4]))
	elseif toks[1] == :with
		return WithNode(parse(toks[2]), parse(toks[3]), parse(toks[4]))
	elseif toks[1] == :lambda
		return LambdaNode(parse(toks[]))
	else
		throw(LispError("Invalid syntax."))
    # in class example
	if length(toks) < 2
		throw(LispError("Too few arguments."))
	elseif length(toks) == 2
		return parse2(toks)
    elseif length(toks) == 3
        return parse3(toks)
	elseif length(toks) == 4
		return parse4(toks)
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

function parse4(toks::Array{Any})
	if toks[1] == :if0
		return If0Node(parse(toks[2]), parse(toks[3]), parse(toks[4]))
	else
		throw(LispError("Wrong token"))
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

function calc(ast::NumNode, env::Environment)
    return NumVal(ast.n)
end

function calc(ast::BinopNode, env::Environment)
	if ast.op == +
		return NumVal(calc(ast.lhs, env) + calc(ast.rhs, env))
	elseif ast.op == -
		return NumVal(calc(ast.lhs, env) - calc(ast.rhs, env))
	elseif ast.op == *
		return NumVal(calc(ast.lhs, env) * calc(ast.rhs, env))
	elseif (ast.op == /) || (ast.op == %)
		rhs = calc(ast.rhs, env)
		if rhs == 0
			throw(LispError("Invalid input: division by zero."))
		elseif ast.op == /
			return NumVal(calc(ast.lhs, env) / calc(ast.rhs, env))
		else
			return NumVal(mod(calc(ast.lhs), calc(ast.rhs)))
		end
	end
end

function calc(ast::UnopNode, env::Environment)
	if ast.op == -
		return NumVal(-1 * calc(ast.arg, env))
	elseif ast.op == collatz
		return NumVal(collatz(calc(ast.arg, env)))
	end
end

function calc(ast::If0Node, env::Environment)
    cond = calc(ast.cond, env)
    if cond.n == 0
        return calc(ast.zerobranch, env)
    else
        return calc(ast.nzerobranch, env)
    end
end

function calc(ast::WithNode, env::Environment)
    binding_val = calc(ast.binding_expr, env)
    ext_env = ExtendedEnv(ast.sym, binding_val, env)
    return calc(ast.body, ext_env)
end

function calc(ast::VarRefNode, env::EmptyEnv)
    throw(Error.LispError("Undefined variable " * string(ast.sym)))
end

function calc(ast::VarRefNode, env::ExtendedEnv)
    if ast.sym == env.sym
        return env.val
    else
        return calc(ast, env.parent)
    end
end

function calc(ast::FuncDefNode, env::Environment)
    return ClosureVal(ast.formal, ast.body , env)
end

function calc(ast::FuncAppNode, env::Environment)
    closure_val = calc(ast.fun_expr, env)
    actual_parameter = calc(ast.arg_expr, env)
    ext_env = ExtendedEnv(closure_val.formal,
                           actual_parameter,
                           closure_val.env)
    return calc(closure_val.body, ext_env)
end

function calc(ast::AE)
    return calc(ast, EmptyEnv())
end

function collatz(n::Real)
	return collatz_helper(n, 0)
end

function collatz_helper(n::Real, num_iters::Int)
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
    return calc(ast, EmptyEnv())
end

end #module
