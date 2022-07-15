structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Type Mismatch!"
val globalEnv: (id * value) list ref = ref []

fun xor(a, b) = if a = b then false else true
fun implies(a, b) = if (a andalso (not b)) then false else true
fun evalExp(e:exp, env:environment):value =
    case e of
	NumExp i            => IntVal i
      | StringExp s         => StringVal s
      | VarExp x            => envLookup (x, env) 				  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | LetExp(ValDecl(x, e1), e2)  =>
  	let
	    val v1 = evalExp (e1, env)
	in
	    evalExp(e2, envAdd (x, v1, env))
        end		   
      | BoolExp b           => BoolVal b
      | UniExp (m, n)       => evalUniExp(m, n, env) 
      | IfExp(b, e1, e2, e3) => evalIfExp(e1, e2, e3, env)  
      | FnExp(x1, _, _, e1) => FnVal(x1, e1, ref (env @ !globalEnv))
      | AppExp(e1, e2) => 
        (case evalExp(e1, env @ !globalEnv ) of 
	      FnVal(x1, body, enclosingEnv) =>
               let val v1 = evalExp(e2, env @ !globalEnv)
               in evalExp(body, envAdd(x1, v1, !enclosingEnv))
	       end
	    |  _ => raise brokenTypes
        )
      | FunExp(fncname, x1, _, _, body) =>
	(
	 let val enclosingEnv = envAdd(fncname, FnVal(x1, body, globalEnv), !globalEnv)
	     val () = globalEnv := enclosingEnv 
	 in FunVal fncname
	 end
	)
and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
case (b, evalExp(e1, env), evalExp(e2, env))  of
      (Add(x), IntVal i1, IntVal i2) => IntVal (i1+i2)
  |   (Sub(x), IntVal i1, IntVal i2) => IntVal (i1-i2)
  |   (Mul(x), IntVal i1, IntVal i2) => IntVal (i1*i2)
  |   (Div, IntVal i1, IntVal i2) => IntVal (i1 div i2)
  |   (Eq(x), IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
  |   (Eq(x), BoolVal i1, BoolVal i2)  => BoolVal (i1 = i2)

  |   (BiTerm, ValList x, y)  => ValList (x@(y::[]))
  |   (Less(x), IntVal i1, IntVal i2)  => BoolVal (i1 < i2)
  |   (Great(x), IntVal i1, IntVal i2)  => BoolVal (i1 > i2)
  |   (And(x), BoolVal i1, BoolVal i2)  => BoolVal (i1 andalso i2)
  |   (Or(x), BoolVal i1, BoolVal i2)  => BoolVal (i1 orelse i2)
  |   (Xor(x), BoolVal i1, BoolVal i2)  => BoolVal (xor(i1, i2))
  |   (Implies(x), BoolVal i1, BoolVal i2)  => BoolVal (implies(i1, i2))

  |   (Eq(x), StringVal s1, StringVal s2) => BoolVal (s1 = s2)
  |   _  => raise brokenTypes  
and
evalUniExp(b:uni, e1:exp, env:environment):value =
case (b, evalExp(e1, env))  of
      (Not(x), BoolVal i1) => BoolVal (not i1)
    | (Neg(x), IntVal i1)  => IntVal (~i1)
    | (Term, x)         => ValList(x::[])
    | _ => raise brokenTypes  
and
evalIfExp(e1:exp, e2:exp, e3:exp, env:environment):value =
case (evalExp(e1, env))  of
      (BoolVal b) => if b then evalExp(e2, env) else evalExp(e3, env)  
      | _ => raise brokenTypes  
					    
end
