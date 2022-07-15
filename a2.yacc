
(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0
 
fun toStr(x) = if x then "TRUE" else "FALSE"

fun rmvTail([]) = []
  | rmvTail(x::[]) = []
  | rmvTail(x::xs) = x::rmvTail(xs)

fun f(v) = 
	let
		val (x,y) = v
	in
		x
	end
	
fun s(v) = 
	let
		val (x,y) = v
	in
		y
	end

fun numToStr(x) = if x < 0 then "-"^Int.toString(~x) else Int.toString(x)
%%
(* required declarations *)
%name Calc

%term
  EOF | TERM | IF of int | RPAREN | LPAREN | THEN | ELSE | IMPLIES of int | OR of int | AND of int | XOR of int | EQUALS of int | NOT of int | ID of string | FI 
  | CONST of bool | TIMES of int | PLUS of int | MINUS of int | GREATERTHAN of int | LESSTHAN of int | NEGATE of int | NUM of int | LET | IN | END | ASSIGN
  | FN | FUN | ARROW | DARROW | COLON | INT | BOOL 

%nonterm program of AST.exp | statement of string * AST.exp | formula of string * AST.exp 
	 | decl of string * AST.decl | Type of string * AST.typ | statements of string * AST.exp 

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)
%right ARROW
%nonassoc DARROW
%nonassoc ASSIGN
%right IF THEN ELSE
%right IMPLIES
%left AND OR EQUALS XOR
%right NOT

%nonassoc GREATERTHAN LESSTHAN 
%left PLUS MINUS
%left TIMES 
%right NEGATE

(* %right *)
  (* %nonassoc*)
%start program

%verbose

%%
program: statements (print("["^f(statements)^"program: statements]\n"); s(statements) ) 

statements: statements formula TERM ( ( f(statements)^f(formula)^"TERM ;,statements: statements formula TERM,", AST.BinExp( AST.BiTerm, s(statements), s(formula) ) ) )
	  | formula TERM ( ( f(formula)^"TERM ;,statements: formula TERM,", AST.UniExp(AST.Term, s(formula)) ) ) 

decl: ID ASSIGN formula ( ( "ID "^ID^",ASSIGN =,"^f(formula)^"decl: ID ASSIGN formula,", AST.ValDecl(ID, s(formula)) ) )

formula: IF formula THEN formula ELSE formula FI( ("IF IF,"^f(formula1)^"THEN THEN,"^f(formula2)^"ELSE ELSE,"^f(formula3)^"FI FI, formula: IF formula THEN formula ELSE formula FI,", AST.IfExp(AST.IfThenElse(IF), s(formula1), s(formula2), s(formula3)) )) 
	 | formula IMPLIES formula( (f(formula1)^"IMPLIES IMPLIES,"^f(formula2)^"formula: formula IMPLIES formula,", AST.BinExp(AST.Implies(IMPLIES), s(formula1), s(formula2)) ))
	 | formula OR formula ( (f(formula1)^"OR OR,"^f(formula2)^"formula: formula OR formula,", AST.BinExp(AST.Or(OR), s(formula1), s(formula2)) )) 
	 | formula EQUALS formula ( (f(formula1)^"EQUALS EQUALS,"^f(formula2)^"formula: formula EQUALS formula,", AST.BinExp(AST.Eq(EQUALS), s(formula1), s(formula2)) )) 
	 | formula XOR formula ( (f(formula1)^"XOR XOR,"^f(formula2)^"formula: formula XOR formula,", AST.BinExp(AST.Xor(XOR), s(formula1), s(formula2)) )) 
	 | formula AND formula ( (f(formula1)^"AND AND,"^f(formula2)^"formula: formula AND formula,", AST.BinExp(AST.And(AND), s(formula1), s(formula2)) ) ) 
         | NOT formula ( ("NOT NOT,"^f(formula)^"formula: NOT formula,", AST.UniExp(AST.Not(NOT), s(formula)) ) ) 
	 | CONST ( ("CONST "^toStr(CONST)^",formula: CONST,", AST.BoolExp(CONST) ) ) 
	 | LPAREN formula RPAREN ( ("LPAREN (,"^f(formula)^"RPAREN ),formula: LPAREN formula RPAREN,", s(formula)) ) 
	 | ID ( ("ID "^ID^",formula: ID,", AST.VarExp(ID)) ) 
	 | NUM ( ("NUM "^numToStr(NUM)^",formula: NUM,", AST.NumExp(NUM)) ) 
	 | formula PLUS formula ( (f(formula1)^"PLUS PLUS,"^f(formula2)^"formula: formula PLUS formula,", AST.BinExp(AST.Add(PLUS), s(formula1), s(formula2)) ) ) 
	 | formula TIMES formula ( (f(formula1)^"TIMES TIMES,"^f(formula2)^"formula: formula TIMES formula,", AST.BinExp(AST.Mul(TIMES), s(formula1), s(formula2)) ) ) 	 
	 | formula MINUS formula ( (f(formula1)^"MINUS MINUS,"^f(formula2)^"formula: formula MINUS formula,", AST.BinExp(AST.Sub(MINUS), s(formula1), s(formula2)) ) ) 
	 | formula LESSTHAN formula ( (f(formula1)^"LESSTHAN LESSTHAN,"^f(formula2)^"formula: formula LESSTHAN formula,", AST.BinExp(AST.Less(LESSTHAN), s(formula1), s(formula2)) ) ) 
         | NEGATE formula ( ("NEGATE NEGATE,"^f(formula)^"formula: NEGATE formula,", AST.UniExp(AST.Neg(NEGATE), s(formula)) ) ) 
	 | LET decl IN formula END ( ( "LET let,"^f(decl)^"IN in,"^f(formula)^"END end, formula: let decl in formula end,", AST.LetExp(s(decl), s(formula)) ) )
	 | formula GREATERTHAN formula ( (f(formula1)^"GREATERTHAN GREATERTHAN,"^f(formula2)^"formula: formula GREATERTHAN formula,", AST.BinExp(AST.Great(GREATERTHAN), s(formula1), s(formula2)) ) ) 
	 
	 | LPAREN formula formula RPAREN ( ("LPAREN (,"^f(formula1)^f(formula2)^"RPAREN ),formula: LPAREN formula formula RPAREN,", AST.AppExp(s(formula1), s(formula2)) ) )
	 | FUN ID LPAREN ID COLON Type RPAREN COLON Type DARROW formula ( ( "FUN FUN, ID "^ID1^",LPAREN (,ID "^ID2^",COLON :,"^f(Type1)^"RPAREN ),COLON :,"^f(Type2)^"DARROW =>,"^f(formula)^"formula: FUN ID LPAREN ID COLON Type RPAREN COLON Type DARROW formula," , AST.FunExp(ID1, ID2, s(Type1) , s(Type2), s(formula) )  ) )  
	 | FN     LPAREN ID COLON Type RPAREN COLON Type DARROW formula ( ( "FN FN,LPAREN (,ID "^ID^",COLON :,"^f(Type1)^"RPAREN ),COLON :,"^f(Type2)^"DARROW =>,"^f(formula)^"formula: FN LPAREN ID COLON Type RPAREN COLON Type DARROW formula," , AST.FnExp(ID, s(Type1), s(Type2), s(formula) ) ) )  
       

Type : Type ARROW Type ( ( f(Type1) ^ "ARROW ->," ^ f(Type2) ^ "Type: Type ARROW Type,", AST.FnTy( s(Type1) , s(Type2) ) ) )
      | INT ( ( "INT int"^",Type: INT," , AST.IntTy)  )
      | BOOL ( ( "BOOL bool"^",Type: BOOL," , AST.BoolTy)  )
      | LPAREN Type RPAREN ( ( "LPAREN (," ^ f(Type) ^ "RPAREN )," ^ "Type: LPAREN Type RPAREN,", s(Type) ) )

