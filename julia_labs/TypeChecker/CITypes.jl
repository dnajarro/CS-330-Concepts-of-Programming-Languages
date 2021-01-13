#
# Class Interpreter - Type Checker
#

module CITypes # based on the CI3 interpreter

push!(LOAD_PATH, pwd())

using Error
using Lexer
export parse, type_of_expr, NumType, BoolType, FuncType, NListType

# ===================================================

abstract type AE
end

abstract type TypeVal
end

# <expr> ::= <number>
struct NumNode <: AE
  n::Real
end

# <expr> ::= true
# <expr> ::= false
struct BooleanNode <: AE
  v::Bool
end

# <expr> ::= (+ <expr> <expr>)
struct PlusNode <: AE
    lhs::AE
    rhs::AE
end

# <expr> ::= (- <expr> <expr>)
struct MinusNode <: AE
    lhs::AE
    rhs::AE
end

# <expr> ::= (iszero <expr>)
struct IsZeroNode <: AE
  arg::AE
end

# <expr> ::= (ifb <expr> <expr> <expr>)
struct IfBNode <: AE
    cond::AE
    zerobranch::AE
    nzerobranch::AE
end

# <expr> ::= (with <id> <expr> <expr>)
struct WithNode <: AE
    sym::Symbol
    binding_expr::AE
    body::AE
end

# <expr> ::= <id>
struct VarRefNode <: AE
    sym::Symbol
end

# <expr> ::= (lambda <id> : <type> <expr>)
struct FuncDefNode <: AE
  formal_parameter::Symbol
  formal_type::TypeVal
  body::AE
end

# <expr> ::= (<expr> <expr>)
struct FuncAppNode <: AE
    fun_expr::AE
    arg_expr::AE
end

# <expr> ::= nempty -> NListType
struct NEmptyNode <: AE
end

# <expr> ::= (nisempty <expr>) -> BoolType
struct NIsEmptyNode <: AE
  list::AE
end

# <expr> ::= (ncons <expr> <expr>) -> NListType
struct NConsNode <: AE
  f::AE
  r::AE
end

# <expr> ::= (nfirst <expr>) -> NumType
struct NFirstNode <: AE
  list::AE
end

# <expr> ::= (nrest <expr>) -> NListType
struct NRestNode <: AE
  list::AE
end

# ===================================================

# <type> ::= number
struct NumType <: TypeVal
end

# <type> ::= boolean
struct BoolType <: TypeVal
end

# <type> ::= (<type> : <type>)
struct FuncType <: TypeVal
  arg_type::TypeVal
  result_type::TypeVal
end

# <type> ::= nlist
struct NListType <: TypeVal
end

# ===================================================

abstract type TypeEnvironment
end

struct EmptyTypeEnv <: TypeEnvironment
end

struct ExtendedTypeEnv <: TypeEnvironment
    sym::Symbol
    val::TypeVal
    parent::TypeEnvironment
end

# ===================================================

# Parser for expressions
# Functional for valid input, doesn't fully reject bad input

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Bool )
  return BooleanNode( expr )
end

function parse( expr::Symbol )
  if expr == :nempty
    return NEmptyNode()
  else
    return VarRefNode( expr )
  end
end

function parse( expr::Array{Any} )

  op_symbol = expr[1]

  if op_symbol == :+
    lhs = parse( expr[2] )
    rhs = parse( expr[3] )
    return PlusNode( lhs, rhs )

  elseif op_symbol == :-
    lhs = parse( expr[2] )
    rhs = parse( expr[3] )
    return MinusNode( lhs, rhs )

  elseif op_symbol == :iszero
    arg = parse( expr[2] )
    return IsZeroNode( arg )

  elseif op_symbol == :ifb
    condition = parse( expr[2] )
    true_branch = parse( expr[3] )
    false_branch = parse( expr[4] )
    return IfBNode( condition, true_branch, false_branch )

  elseif op_symbol == :with
    sym = expr[2]
    binding_expr = parse( expr[3] )
    body = parse( expr[4] )
    return WithNode( sym, binding_expr, body )

  elseif op_symbol == :lambda
    formal = expr[2]
    formal_type = parse_type(expr[4])
    body = parse(expr[5])
    return FuncDefNode( formal, formal_type, body )

  elseif op_symbol == :ncons
    f = parse(expr[2])
    r = parse(expr[3])
    return NConsNode( f, r )

  elseif op_symbol == :nisempty
    list = parse(expr[2])
    return NIsEmptyNode( list )

  elseif op_symbol == :nfirst
    list = parse(expr[2])
    return NFirstNode( list )

  elseif op_symbol == :nrest
    list = parse(expr[2])
    return NRestNode( list )

  else
    return FuncAppNode( parse(expr[1]), parse(expr[2]) )

  end
end

function parse( expr::Any )
  throw( LispError("Invalid expression $expr") )
end

# ===================================================

# Parser for type expressions

function parse_type( t::Symbol )
  if (t == :number)
    return NumType()
  elseif (t == :boolean)
    return BoolType()
  elseif (t == :nlist)
    return NListType()
  end
end

function parse_type( t :: Array{Any} )
  return FuncType( parse_type(t[1]),
                  parse_type(t[3]))
end

function parse_type( expr::Any )
  throw( LispError("Invalid type $expr") )
end

# ===================================================

# Type checking functions (modeled after the earlier calc)

function type_of_expr( ast::AE )
  return type_of_expr( ast, EmptyTypeEnv() )
end

function type_of_expr( ast::NumNode, env::TypeEnvironment )
  return NumType()
end

function type_of_expr( ast::BooleanNode, env::TypeEnvironment )
  return BoolType()
end

function type_of_expr( ast::PlusNode, env::TypeEnvironment )
  left = type_of_expr( ast.lhs, env )
  right = type_of_expr( ast.rhs, env )
  return type_of_math_expr( left, right )
end

function type_of_expr( ast::MinusNode, env::TypeEnvironment )
  left = type_of_expr( ast.lhs, env )
  right = type_of_expr( ast.rhs, env )
  return type_of_math_expr( left, right )
end

# the rest of your type-checking functions go here...

function type_of_expr(ast::IsZeroNode, env::TypeEnvironment)
  arg = type_of_expr(ast.arg, env)
  return type_of_iszero_expr(arg)
end

function type_of_expr(ast::IfBNode, env::TypeEnvironment)
  cond = type_of_expr(ast.cond, env)
  left = type_of_expr(ast.zerobranch, env)
  right = type_of_expr(ast.nzerobranch, env)
  return type_of_ifb_expr(cond, left, right)
end

function type_of_expr(sym::Symbol, env::ExtendedTypeEnv)
  return env.val
end

function type_of_expr(sym::Symbol, env::Any)
  throw(LispError("Symbol is undefined"))
end

function type_of_expr(ast::VarRefNode, env::TypeEnvironment)
  sym = type_of_expr(ast.sym, env)
  return type_of_sym_expr(sym)
end

function type_of_expr(ast::WithNode, env::TypeEnvironment)
  initialization_expr = type_of_expr(ast.binding_expr, env)
  sym = ast.sym
  ext_env = ExtendedTypeEnv(sym, initialization_expr, env)
  body = type_of_expr(ast.body, ext_env)
  return type_of_with_expr(body)
end

function type_of_expr(ast::FuncAppNode, env::TypeEnvironment)
  fun_expr = type_of_expr(ast.fun_expr, env)
  arg_expr = type_of_expr(ast.arg_expr, env)
  return type_of_funapp_expr(fun_expr, arg_expr)
end

function type_of_expr(formal_parameter::TypeVal, env::TypeEnvironment)
  return formal_parameter
end

# TODO: there are some potential problems with the fact that formal_type may be
# FuncType
function type_of_expr(ast::FuncDefNode, env::TypeEnvironment)
  formal_parameter = ast.formal_parameter
  formal_type = ast.formal_type
  ext_env = ExtendedTypeEnv(formal_parameter, formal_type, env)
  body = type_of_expr(ast.body, ext_env)
  return type_of_fundef_expr(formal_type, body)
end

function type_of_expr(ast::NEmptyNode, env::TypeEnvironment)
  return NListType()
end

function type_of_expr(ast::NIsEmptyNode, env::TypeEnvironment)
  list = type_of_expr(ast.list, env)
  return type_of_isempty_expr(list)
end

function type_of_expr(ast::NConsNode, env::TypeEnvironment)
  f = type_of_expr(ast.f, env)
  r = type_of_expr(ast.r, env)
  return type_of_cons_expr(f, r)
end

function type_of_expr(ast::NFirstNode, env::TypeEnvironment)
  list = type_of_expr(ast.list, env)
  return type_of_first_expr(list)
end

function type_of_expr(ast::NRestNode, env::TypeEnvironment)
  list = type_of_expr(ast.list, env)
  return type_of_rest_expr(list)
end

# ===================================================

# Helper function for comparing two type values recursively if necessary

same_type( t1::FuncType, t2::FuncType ) =
    (same_type( t1.arg_type, t2.arg_type )
  && same_type( t1.result_type, t2.result_type ))

same_type( t1::T, t2::T ) where {T <: TypeVal} = true

same_type( t1::TypeVal, t2::TypeVal ) = false

# ===================================================

# Type judgments (could be folded into type checking functions)

function type_of_math_expr( left::NumType, right::NumType )
  return NumType()
end

function type_of_math_expr( left::Any, right::Any )
  throw( LispError("Operands for + or - must be numbers") )
end

# the rest of your type-judgement functions (if you choose to separate them) go here...

function type_of_iszero_expr(arg::NumType)
  return BoolType()
end

function type_of_iszero_expr(arg::Any)
  throw(LispError("iszero function requires a number for an argument"))
end

function type_of_ifb_expr(cond::BoolType, left::Any, right::Any)
  if same_type(left, right)
    return left
  else throw(LispError("Left and right branches of ifb function must have same type"))
  end
end

function type_of_ifb_expr(cond::Any, left::Any, right::Any)
  throw(LispError("Condition expression of ifb must by boolean"))
end

function type_of_with_expr(body::TypeVal)
  return body
end

function type_of_sym_expr(sym::TypeVal)
  return sym
end

function type_of_sym_expr(sym::Any)
  throw(LispError("Invalid symbol type"))
end

function type_of_with_expr(body::Any)
  throw(LispError("Body has invalid type"))
end

function type_of_funapp_expr(fun_expr::FuncType, arg_expr::TypeVal)
  if same_type(fun_expr.arg_type, arg_expr)
    return fun_expr.result_type
  else throw(LispError("Expected function parameter type doesn't match given parameter type"))
  end
end

function type_of_funapp_expr(fun_expr::TypeVal, arg_expr::TypeVal)
  if same_type(fun_expr, arg_expr)
    return fun_expr
  end
end

function type_of_funapp_expr(fun_expr::Any, arg_expr::Any)
  throw(LispError("Invalid function or function argument"))
end

function type_of_fundef_expr(formal_type::TypeVal, body::TypeVal)
  return FuncType(formal_type, body)
end

function type_of_fundef_expr(formal_type::Any, body::Any)
  throw(LispError("Invalid argument or return type(s)"))
end

function type_of_isempty_expr(list::NListType)
  return BoolType()
end

function type_of_isempty_expr(list::Any)
  throw(LispError("The list must have NListType"))
end

function type_of_cons_expr(f::NumType, r::NListType)
  return NListType()
end

function type_of_cons_expr(f::Any, r::Any)
  throw(LispError("The first argument of ncons must be a number and the second must be a list"))
end

function type_of_first_expr(list::NListType)
  return NumType()
end

function type_of_first_expr(list::Any)
  throw(LispError("List must be NListType"))
end

function type_of_rest_expr(list::NListType)
  return NListType()
end

function type_of_rest_expr(list::Any)
  return NListType("List must be NListType")
end

# ===================================================

# convenience function to make everything easier
function type_of_expr( expr::AbstractString )
  return type_of_expr( parse( Lexer.lex(expr) ) )
end

# evaluate a series of tests in a file
function typef( fn::AbstractString )
  f = open( fn )

  cur_prog = ""
  for ln in eachline(f)
      ln = chomp( ln )
      if length(ln) == 0 && length(cur_prog) > 0
          println( "" )
          println( "--------- Evaluating ----------" )
          println( cur_prog )
          println( "---------- Returned -----------" )
          try
              println( type_of_expr( cur_prog ) )
          catch errobj
              println( ">> ERROR: lxd" )
              lxd = Lexer.lex( cur_prog )
              println( lxd )
              println( ">> ERROR: ast" )
              ast = parse( lxd )
              println( ast )
              println( ">> ERROR: rethrowing error" )
              throw( errobj )
          end
          println( "------------ done -------------" )
          println( "" )
          cur_prog = ""
      else
          cur_prog *= ln
      end
  end

  close( f )
end

# ===================================================

end # module
