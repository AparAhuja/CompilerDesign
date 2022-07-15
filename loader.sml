CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast.sml";
use "typing.sml";
use "evaluator.sml";
use "a2.yacc.sig";
use "a2.yacc.sml";
use "a2.lex.sml";
use "a2loader.sml";
Control.Print.printLength := 2000; (* set printing parameters so that *)
Control.Print.printDepth := 2000; (* we’ll see all details *)
Control.Print.stringDepth := 2000; (* and strings *)
