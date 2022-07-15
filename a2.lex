structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  fun rmvTail([]) = []
  | rmvTail(x::[]) = []
  | rmvTail(x::xs) = x::rmvTail(xs)

  val out = ref ""
  val col = ref 1
  val pos = ref 1
  val eof = fn () => let val x = !pos in (TextIO.output(TextIO.stdOut,"["^implode(rmvTail(rmvTail(explode(!out))))^"]\n");  pos := 1; col := 1; out := ""; Tokens.EOF(x, x)) end
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
  fun len([]) = 0
  | len(x::xs) = 1 + len(xs)

%%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1;col := 1; lex());
\r\n       => (pos := (!pos) + 1;col := 1; lex());
{ws}+    => (col := (!col) + len(explode(yytext));lex());


{digit}+ => (out := (!out)^"NUM \""^yytext^"\", ";col := (!col) + len(explode(yytext));
	     Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !pos, !pos));
"TIMES"      => (out := (!out)^"TIMES \""^yytext^"\", ";col := (!col) + 5;Tokens.TIMES(!pos,!pos,!pos));
"PLUS"      => (out := (!out)^"PLUS \""^yytext^"\", ";col := (!col) + 4;Tokens.PLUS(!pos,!pos,!pos));
"MINUS"      => (out := (!out)^"MINUS \""^yytext^"\", ";col := (!col) + 5;Tokens.MINUS(!pos,!pos,!pos));
"GREATERTHAN"      => (out := (!out)^"GREATERTHAN \""^yytext^"\", ";col := (!col) + 11;Tokens.GREATERTHAN(!pos,!pos,!pos));
"LESSTHAN"      => (out := (!out)^"LESSTHAN \""^yytext^"\", ";col := (!col) + 8;Tokens.LESSTHAN(!pos,!pos,!pos));
"NEGATE"      => (out := (!out)^"NEGATE \""^yytext^"\", ";col := (!col) + 6;Tokens.NEGATE(!pos,!pos,!pos));

"fn"      => (out := (!out)^"FN \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.FN(!pos,!col));
"fun"      => (out := (!out)^"FUN \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.FUN(!pos,!col));
"->"      => (out := (!out)^"ARROW \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.ARROW(!pos,!col));
"=>"      => (out := (!out)^"DARROW \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.DARROW(!pos,!col));
":"      => (out := (!out)^"COLON \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.COLON(!pos,!col));
"int"      => (out := (!out)^"INT \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.INT(!pos,!col));
"bool"      => (out := (!out)^"BOOL \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.BOOL(!pos,!col));

"("      => (out := (!out)^"LPAREN \""^yytext^"\", "; col := (!col) + 1; Tokens.LPAREN(!pos,!col));
")"      => (out := (!out)^"RPAREN \""^yytext^"\", ";col := (!col) + 1;Tokens.RPAREN(!pos,!col));
";"      => (out := (!out)^"TERM \""^yytext^"\", ";col := (!col) + 1;Tokens.TERM(!pos,!col));
"="      => (out := (!out)^"ASSIGN \""^yytext^"\", ";col := (!col) + 1;Tokens.ASSIGN(!pos,!col));
"TRUE"      => (out := (!out)^"CONST \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.CONST(true,!pos,!col));
"FALSE"      => (out := (!out)^"CONST \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.CONST(false,!pos,!col));

"NOT"      => (out := (!out)^"NOT \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.NOT(!pos,!pos,!col));
"AND"      => (out := (!out)^"AND \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.AND(!pos,!pos,!col));
"OR"      => (out := (!out)^"OR \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.OR(!pos,!pos,!col));
"XOR"      => (out := (!out)^"XOR \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.XOR(!pos,!pos,!col));
"EQUALS"      => (out := (!out)^"EQUALS \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.EQUALS(!pos,!pos,!col));
"IMPLIES"      => (out := (!out)^"IMPLIES \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.IMPLIES(!pos,!pos,!col));

"if"      => (out := (!out)^"IF \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.IF(!pos,!pos,!col));
"then"      => (out := (!out)^"THEN \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.THEN(!pos,!col));
"else"      => (out := (!out)^"ELSE \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.ELSE(!pos,!col));
"fi"      => (out := (!out)^"FI \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.FI(!pos,!col));
"let"      => (out := (!out)^"LET \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.LET(!pos,!col));
"in"      => (out := (!out)^"IN \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.IN(!pos,!col));
"end"      => (out := (!out)^"END \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.END(!pos,!col));

{alpha}+ => (out := (!out)^"ID \""^yytext^"\", ";col := (!col) + len(explode(yytext));Tokens.ID(yytext,!pos,!col));
.      => (error ("Unknown token:"^Int.toString(!pos)^":"^Int.toString(!col)^":"^yytext^".",!pos,!col); lex());

