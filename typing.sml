structure Typing =
struct
open AST

type typEnv = (id * typ ) list
val globalTyEnv: (id * typ ) list ref = ref []

fun typEnvLookup (var :id , env :typEnv) : typ =
      case List.find( fn (x, _ )  => x= var ) env of
        SOME (x,v) => v
        | NONE => raise Fail ("Variable " ^ var ^ " is w/o a type" )

fun typEnvAdd (var :id, t:typ , env :typEnv): typEnv =
    (var , t ) :: env

fun t2s(x) =
    case (x) of
     IntTy => "Int"
     | BoolTy => "Bool"
     | FnTy(t1, t2) => "Fn"
     | StatTy => "Statement"
     | StrTy => "String"

fun getType (e :exp , env: typEnv) : typ =
  case e of
    NumExp _ => IntTy
    | BoolExp _ => BoolTy
    | StringExp _ => StrTy
    | BinExp ( operation , e1 , e2 ) =>
    (
     case (operation, getType(e1, env), getType(e2, env)) of
        (Add(x), IntTy,IntTy ) => IntTy
    |   (Sub(x), IntTy,IntTy ) => IntTy
    |   (Mul(x), IntTy,IntTy ) => IntTy
    |   (Eq(x), IntTy,IntTy ) => BoolTy
    |   (Eq(x), BoolTy, BoolTy ) => BoolTy
    |   (Less(x), IntTy,IntTy ) => BoolTy
    |   (Great(x), IntTy,IntTy ) => BoolTy
    |   (And(x), BoolTy, BoolTy)  => BoolTy
    |   (Or(x), BoolTy, BoolTy)  => BoolTy
    |   (Xor(x), BoolTy, BoolTy)  => BoolTy
    |   (Implies(x),BoolTy,BoolTy)  => BoolTy
    |   (BiTerm, StatTy, _ ) => StatTy   
    |   (Add(x), a, b ) => raise Fail (" In Add BinExp types mismatch. Line: "^Int.toString(x)^". Expected: Int * Int. Given: " ^t2s(a)^" * "^t2s(b) )
    |   (Sub(x), a, b) => raise Fail (" In Sub BinExp types mismatch. Line: "^Int.toString(x)^". Expected: Int * Int. Given: " ^t2s(a)^" * "^t2s(b))
    |   (Mul(x), a, b) => raise Fail (" In Mul BinExp types mismatch. Line: "^Int.toString(x)^". Expected: Int * Int. Given: " ^t2s(a)^" * "^t2s(b))
    |   (Eq(x), a, b) => raise Fail (" In Eq BinExp types mismatch. Line: "^Int.toString(x)^". Expected: Int * Int or Bool * Bool. Given: " ^t2s(a)^" * "^t2s(b))
    |   (Less(x), a, b) => raise Fail (" In Less BinExp types mismatch. Line: "^Int.toString(x)^". Expected: Int * Int. Given: " ^t2s(a)^" * "^t2s(b))
    |   (Great(x), a, b) => raise Fail (" In Great BinExp types mismatch. Line: "^Int.toString(x)^". Expected: Int * Int. Given: " ^t2s(a)^" * "^t2s(b))
    |   (And(x), a, b)  => raise Fail (" In And BinExp types mismatch. Line: "^Int.toString(x)^". Expected: Bool * Bool. Given: " ^t2s(a)^" * "^t2s(b))
    |   (Or(x), a, b)  => raise Fail (" In Or BinExp types mismatch. Line: "^Int.toString(x)^". Expected: Bool * Bool. Given: " ^t2s(a)^" * "^t2s(b))
    |   (Xor(x), a, b)  => raise Fail (" In Xor BinExp types mismatch. Line: "^Int.toString(x)^". Expected: Bool * Bool. Given: " ^t2s(a)^" * "^t2s(b))
    |   (Implies(x), a, b)  => raise Fail (" In Implies BinExp types mismatch. Expected: Bool * Bool. Given: " ^t2s(a)^" * "^t2s(b))
    |   _  => raise Fail (" In BinExp types mismatch ")
    )
    | UniExp ( operation , e ) => 
    (
    case (operation, getType(e, env)) of
         (Not(x), BoolTy) => BoolTy
    |    (Neg(x), IntTy)  => IntTy
    |    (Term, _ )    => StatTy 
    |    (Not(x), a) => raise Fail (" In Not UniExp types mismatch. Line: "^Int.toString(x)^". Expected: Bool. Given: "^t2s(a) )
    |    (Neg(x), a)  => raise Fail (" In Neg UniExp types mismatch. Line: "^Int.toString(x)^". Expected: Int. Given: "^t2s(a) )
    )
    | VarExp x => typEnvLookup (x, env)
    | AppExp (e1 , e2 ) =>
      ( case(getType(e1 , env @ !globalTyEnv ), getType(e2, env @ !globalTyEnv ) ) of
        ( FnTy (t1 , t2 ), t3 ) =>
        if  t1 = t3
          then t2
          else raise Fail ("Application argument type mismatch in AppExp. Expected: "^t2s(t1)^". Given: "^t2s(t3))
        | ( x , y ) => raise Fail ("Function was expected in AppExp. Given: "^t2s(x) )
      )
    | FnExp (x , t1 , t2 , e) =>
          let val computedType = getType (e,typEnvAdd(x,t1,env))
	  in
	    if (t2 <> computedType) then raise Fail (" Type Mismatch in FnExp. Expected: "^t2s(t2)^". Given: "^t2s(computedType)) else FnTy (t1 , t2)
	  end
    | IfExp (IfThenElse(x), e1 , e2 , e3 ) =>
      (
        let
          val t1 = getType(e1,env)
          val t2 = getType(e2,env)
          val t3 = getType(e3,env)
        in
          if t1 <> BoolTy
          then raise Fail ("Condition of IfThenElse is not of BoolTy. Line: "^Int.toString(x)^". Expected: Bool. Given: "^t2s(t1))
          else
            if t2 <> t3
            then raise Fail ("IfThenElse branches have different types. Line: "^Int.toString(x)^". Given: "^t2s(t2)^" | "^t2s(t3))
            else
            t2
          end
        )
      | LetExp(ValDecl(x, e1), e2) => getType(e2,typEnvAdd(x, getType(e1,env),env))
      | FunExp (fname , x1 , t1, t2 , e) =>
      (
        let
            val eTyp = getType (e,typEnvAdd(fname,FnTy(t1, t2),typEnvAdd(x1,t1,env)))
	    val () = globalTyEnv := (fname, FnTy(t1, t2)) :: !globalTyEnv
          in
           if eTyp <> t2
           then raise Fail ("Mismatch in declared type and actual type in function: "^fname^". Declared: "^t2s(t2)^". Actual: "^t2s(eTyp))
           else FnTy(t1, t2)
          end
        )
      end
