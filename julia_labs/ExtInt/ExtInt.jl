module ExtInt

push!(LOAD_PATH, pwd())

using Error
using Lexer
export parse, calc, interp

#
# ==================================================
#

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

# <AE> ::= (<AE> <AE>*) FuncAppNode is a function expression followed by an argument expression, i.e., used to represent any function
struct FunAppNode <: AE
    fun_expr::AE
    arg_expr::Array{AE}
end

# <AE> ::= (with <id> <AE> <AE>)
# (with ((some list of ids bound to some value, i.e., x = 5, written as x 5)) (some function/action that you want to do with those variables you just defined, i.e., (+ x 3)))
struct WithNode <: AE
	binding_exprs::Array{FunAppNode} # the (x 5) (y 3) etc. in the with node
 	body::AE # the place where the variable gets used in some expression, e.g. (+ x 4)
end

# <AE> ::= <id> VarRefNode is an id; i.e., symbol associated with a variable
struct VarRefNode <: AE
	sym::Symbol
end

# <AE> ::= (lambda <id> <AE>) FuncDefNode is a symbol bound to a body (which could be a NumNode, WithNode, BinopNode, etc.)
struct FunDefNode <: AE
	fun_def_list::Array{VarRefNode}
 	fun_body::AE
end

#
# ==================================================
#

abstract type RetVal
end

abstract type Environment
end

struct NumVal <: RetVal
    n::Real
end

struct ClosureVal <: RetVal
	formals::Array{VarRefNode}
    body::AE
    env::Environment
end

#
# ==================================================
#

struct EmptyEnv <: Environment
end

struct ExtendedEnv <: Environment
    syms_to_vals::Array{Tuple{VarRefNode, RetVal}}
    parent::Environment
end

#
# ==================================================
#

function collatz(n::Real)
	return collatz_helper(n, 0)
end

function collatz_helper(n::Real, num_iters::Int)
	if n <= 0
		throw(LispError("Invalid input. Collatz function needs a positive number."))
	elseif n == 1
		return num_iters
	elseif mod(n, 2) == 0
		return collatz_helper(n / 2, num_iters + 1)
	else
		return collatz_helper(3 * n + 1, num_iters + 1)
	end
end

dict = Dict(
	:+ => +,
	:- => -,
	:* => *,
	:/ => /,
	:mod => %,
	:collatz => collatz,
	:if0 => -1,
	:with => -2,
	:lambda => -3
)

#
# =================================================
#

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Symbol )
	if haskey(dict, expr) == true
		throw(LispError("Syntax error: cannot define id as keyword"))
	end
    return VarRefNode( expr )
end

function parse( expr::Array{Any} )
	if length(expr) < 1
		throw(LispError("Syntax error: invalid empty list"))
    elseif expr[1] == :+
        if length(expr) != 3
            throw(LispError("Syntax error: need 2 values after function name for addition"))
        else
            return BinopNode(dict[expr[1]], parse( expr[2] ), parse( expr[3] ) )
        end
    elseif expr[1] == :-
        if length(expr) != 3 && length(expr) != 2
            throw(LispError("Syntax error: need 1 or 2 values after function name for this function"))
        elseif length(expr) == 3
            return BinopNode(dict[expr[1]], parse( expr[2] ), parse( expr[3] ) )
		elseif length(expr) == 2
			return UnopNode(dict[expr[1]], parse(expr[2]))
        end
	elseif expr[1] == :*
	        if length(expr) != 3
	            throw(LispError("Syntax error: need 2 values after function name for addition"))
	        else
	            return BinopNode(dict[expr[1]], parse( expr[2] ), parse( expr[3] ) )
	        end
	elseif expr[1] == :/
        if length(expr) != 3
            throw(LispError("Syntax error: need 2 values after function name for addition"))
        else
            return BinopNode(dict[expr[1]], parse( expr[2] ), parse( expr[3] ) )
        end
    elseif expr[1] == :mod
        if length(expr) != 3
            throw(LispError("Syntax error: need 2 values after function name for this function"))
        else
            return BinopNode(dict[expr[1]], parse(expr[2]), parse(expr[3]))
		end
    elseif expr[1] == :collatz
        if length(expr) != 2
            throw(LispError("Syntax error: need 1 value after function name for this function"))
        else
            return UnopNode(dict[expr[1]], parse(expr[2]))
		end
    elseif expr[1] == :if0
        if length(expr) != 4
            throw(LispError("Syntax error: need 3 values after function name"))
        else
            return If0Node( parse(expr[2]), parse(expr[3]) , parse(expr[4]) )
        end
    elseif expr[1] == :with
		if length(expr) < 3
			throw(LispError("Syntax error: need 2 arguments after function name"))
		end

		if isa(expr[2], Array) == false
			throw(LispError("Syntax error: first parameter in with function must be a list"))
		end

		sym_bindings = FunAppNode[]
		identifiers = Set{Symbol}()
		for binding in expr[2]
			if isa(binding, Array) == false
				throw(LispError("Syntax error: expected a list of lists for first parameter"))
			elseif length(binding) < 2
				throw(LispError("Syntax error: missing id and/or binding value"))
			elseif haskey(dict, binding[1]) == true
				throw(LispError("Syntax error: cannot redefine keywords"))
			elseif in(binding[1], identifiers) == true
				throw(LispError("Syntax error: duplicate identifier"))
			else
				push!(identifiers, binding[1])
			end

			id = parse(binding[1])
			binding_expr = parse(binding[2])
			push!(sym_bindings, FunAppNode(id, AE[binding_expr]))
		end

        return WithNode( sym_bindings, parse(expr[3]) )
    elseif expr[1] == :lambda
		if length(expr) < 3
			throw(LispError("Syntax error: lambda expects a list of parameters and function definition"))
		end
		identifiers = Set{Symbol}()
		vars = VarRefNode[]
		if isa(expr[2], Array) == false
			throw(LispError("Syntax error: first parameter in lambda function must be a list"))
		end
		for binding in expr[2]
			if haskey(dict, binding)
				throw(LispError("Syntax error: cannot redefine keywords"))
			elseif in(binding, identifiers)
				throw(LispError("Syntax error: duplicate identifier"))
			elseif isa(binding, Symbol) != true
				throw(LispError("Syntax error: lambda expects list of identifiers"))
			else
				push!(identifiers, binding)
				push!(vars, parse(binding))
			end
		end

        return FunDefNode( vars, parse(expr[3]) )
    else
		args = AE[]
		for arg in expr[2:end]
			push!(args, parse(arg))
		end
		return FunAppNode( parse(expr[1]), args )
    end

    throw(LispError("Unknown operator!"))
end

function parse( expr::Any )
	throw(LispError("Invalid type $expr") )
end

#
# ==================================================
#

function calc( ast::NumNode, env::Environment )
    if isa(ast.n, Real) != true
        throw(LispError("Type error: number is not a Real"))
    else
        return NumVal( ast.n )
    end
end

function calc( ast::BinopNode, env::Environment )
    lhs = calc( ast.lhs, env )
	if isa(lhs, NumVal) != true
		throw(LispError("Type error: left hand argument has wrong type"))
	end
    rhs = calc( ast.rhs, env )
	if isa(rhs, NumVal) != true
		throw(LispError("Type error: right hand argument has wrong type"))
	end
	if ast.op == +
		return NumVal(calc(ast.lhs, env).n + calc(ast.rhs, env).n)
	elseif ast.op == -
		return NumVal(calc(ast.lhs, env).n - calc(ast.rhs, env).n)
	elseif ast.op == *
		return NumVal(calc(ast.lhs, env).n * calc(ast.rhs, env).n)
	elseif (ast.op == /) || (ast.op == %)
		rhs = calc(ast.rhs, env)
		if isa(rhs, NumVal) != true
			throw(LispError("Type error: parameter(s) with wrong type"))
		elseif rhs.n == 0
			throw(LispError("Invalid input: division by zero."))
		elseif ast.op == /
			return NumVal(calc(ast.lhs, env).n / calc(ast.rhs, env).n)
		else
			return NumVal(mod(calc(ast.lhs, env).n, calc(ast.rhs, env).n))
		end
	end
end

function calc(ast::UnopNode, env::Environment)
	arg = calc(ast.arg, env)
	if isa(arg, NumVal) != true
		throw(LispError("Type error: collatz/minus operator expect a number"))
	end
	if ast.op == -
		return NumVal(-1 * calc(ast.arg, env).n)
	elseif ast.op == collatz
		return NumVal(collatz(calc(ast.arg, env).n))
	end
end

function calc( ast::If0Node, env::Environment )
    cond = calc(ast.cond)
	if isa(cond, NumVal) != true
		throw(LispError("Type error: conditional test statement has wrong type"))
	end

    if cond.n == 0
        return calc( ast.zerobranch, env )
    else
		return calc(ast.nzerobranch, env)
    end
end

function calc( ast::WithNode, env::Environment )
	syms_to_vals = Array{Tuple{VarRefNode, RetVal}}(undef, 0)
	for expr in ast.binding_exprs
    	binding_val = calc( expr.arg_expr[1], env ) # evaluate the expression that gets bound to the id
		push!(syms_to_vals, (expr.fun_expr, binding_val))
	end
	ext_env = ExtendedEnv(syms_to_vals, env)
	return calc( ast.body, ext_env ) # then we evaluate the body of the with statement with those new values
end

function calc( ast::VarRefNode, env::EmptyEnv )
    throw( Error.LispError("Undefined variable " * string( ast.sym )) )
end

function calc( ast::VarRefNode, env::ExtendedEnv )
	for sym_tuple in env.syms_to_vals
		if ast.sym == sym_tuple[1].sym
			return sym_tuple[2]
		end
    end

	return calc(ast, env.parent)
end

function calc( ast::FunDefNode, env::Environment )
    return ClosureVal( ast.fun_def_list, ast.fun_body , env )
end

function calc( ast::FunAppNode, env::Environment )
	if isa(ast.fun_expr, NumNode) == true
		throw(LispError("Type error: functions cannot be numbers"))
	end
    closure_val = calc( ast.fun_expr, env )
	if isa(closure_val, ClosureVal) != true
		throw(LispError("Type error: FunAppNode must evaluate to ClosureVal"))
	else
		len = length(closure_val.formals)
		if len != length(ast.arg_expr)
			throw(LispError("Arity error: this function requires $len parameters"))
		end
	end

	syms_to_vals = Array{Tuple{VarRefNode, RetVal}}(undef, 0)
	for i = 1:len
		binding_val = calc(ast.arg_expr[i], env)
		push!(syms_to_vals, (closure_val.formals[i], binding_val))
	end

	ext_env = ExtendedEnv( syms_to_vals, closure_val.env )
	closure = calc(closure_val.body, ext_env)
	return closure
end

function calc( ast::AE )
    return calc( ast, EmptyEnv() )
end

#
# ==================================================
#

function interpf( cs::AbstractString )
	lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast, EmptyEnv() )
end

# evaluate a series of tests in a file
# function interpf( fn::AbstractString )
#   f = open( fn )
#
#   cur_prog = ""
#   for ln in eachline(f)
#       ln = chomp( ln )
#       if length(ln) == 0 && length(cur_prog) > 0
#           println( "" )
#           println( "--------- Evaluating ----------" )
#           println( cur_prog )
#           println( "---------- Returned -----------" )
#           try
#               println( interp( cur_prog ) )
#           catch errobj
#               println( ">> ERROR: lxd" )
#               lxd = Lexer.lex( cur_prog )
#               println( lxd )
#               println( ">> ERROR: ast" )
#               ast = parse( lxd )
#               println( ast )
#               println( ">> ERROR: rethrowing error" )
#               throw( errobj )
#           end
#           println( "------------ done -------------" )
#           println( "" )
#           cur_prog = ""
#       else
#           cur_prog *= ln
#       end
#   end
#
#   close( f )
# end

end #module
