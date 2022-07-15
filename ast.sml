structure AST =
struct

type id = string

datatype binop = Add of int | Sub of int | Mul of int | Div | Eq of int| Less of int | Great of int
		| And of int | Xor of int | Or of int | Implies of int | BiTerm
datatype tern = IfThenElse of int
datatype uni = Not of int | Neg of int | Term

datatype typ = IntTy
              | BoolTy
              | StrTy
	      | StatTy
              | FnTy of typ * typ

datatype decl = ValDecl of id * exp

and exp =  NumExp of int
    	| StringExp of string
    	| VarExp of id
	| BinExp of binop * exp * exp
	| LetExp of decl * exp
        | BoolExp of bool
	| UniExp of uni * exp
	| IfExp of tern * exp * exp * exp
	| AppExp of exp * exp
        | FnExp of id * typ * typ * exp
        | FunExp of id * id * typ * typ * exp
				     				
datatype value = IntVal of int
               | StringVal of string
	       | BoolVal of bool
	       | ValList of value list | FunVal of string
	       | FnVal of id * exp * ( (id * value) list ref ) 
	       | TypeError

type environment = (id * value) list


fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"							    
end


