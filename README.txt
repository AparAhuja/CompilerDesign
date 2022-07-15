Type the following commands and hit enter in terminal to run the code -

1. ml-lex a2.lex
2. ml-yacc a2.yacc 
3. sml
4. use "loader.sml"
5. read "fileName.txt"

NOTES:
- Add the string to be parsed and evaluated in "fileName.txt"
- Each sentence of the program in the file must end with the terminal (";") symbol. (including the last sentence)
- Output: 
	- The File content, Lexer output and Parser output is printed.
	- The AST and Program-Evaluation are stored in "it" and displayed in terminal
	- The Program-Evaluation is a list of values. Each value represents the value of each statement.
- Keywords: TIMES PLUS MINUS GREATERTHAN LESSTHAN NEGATE 
            fn fun int bool TRUE FALSE NOT AND OR XOR 
            EQUALS IMPLIES if then else fi let in end
