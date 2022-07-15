structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
structure CalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = CalcLrVals.ParserData
     	       structure Lex = CalcLex)
val flag = ref true;
val rule = "formula -> IF formula THEN formula ELSE formula | formula IMPLIES formula | formula OR formula | formula EQUALS formula  | formula XOR formula  | formula AND formula  | NOT formula | CONST | LPAREN formula RPAREN | ID";
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,col:int) =
		    	(if !flag then (TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString pos) ^ ":" ^ (Int.toString col) ^ ":ProductionError\n"); flag := false) else () ) 
			(* (if !flag then (TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString pos) ^ ":" ^ (Int.toString col) ^ ":"^rule^"\n"); flag := false) else () ) *)
		in
		    CalcParser.parse(0,lexstream,print_error,())
		end
		

fun stringToLexer str =
    let val done = ref false
    	val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = CalcParser.Stream.get lexer
	val out = ref ""
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end
open EVALUATOR
open Typing
val parseString = parse o stringToLexer
fun typeCheckAndEval (x) = if getType(x, []) = StatTy then evalExp(x, []) else TypeError

fun read file = let val inStream = TextIO.openIn file
                    val str = TextIO.inputAll inStream
		    val () = print("File Content: \"" ^ str ^ "\"\n")
		    val temp = parseString str
                in (temp, typeCheckAndEval(temp)) end
handle ParseError => (OS.Process.exit OS.Process.success : unit; (parseString(""), IntVal(~1)))