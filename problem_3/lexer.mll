	{
        open Parser        (* The type token is defined in parser.mli *)
        exception Eof
  }
  rule token = parse
            [' ' '\t' '\n' '\r']     { token lexbuf }     (* skip blanks *)
          |  "//"[' '-'~']*'\n'     {token lexbuf} (* skip comments *)
          | '+'            { PLUS }
          | '-'            { MINUS }
          | '*'            { TIMES }
          | '&'            { AND }
	        | '|'            { OR }
          | '<'            { LT }
          | '='            { EQ }
          | "=="           { EQQ}
	        | "!="           { NEQQ}
          | '>'            { GT }
          | "<="           { LE }
          | ">="           { GE }
          | '!'            { NOT }
          | '('            { LPAREN }
          | ')'            { RPAREN }
	        | '{'            { LCURL }
	        | '}'            { RCURL }
          | '['            { LSQR }
          | ']'            { RSQR }
	        | ';'            { SEMI }
          | "if"           { IF }
          | "else"         { ELSE }
          | "while"        { WHILE }
          | "Pre"          { PRE }
          | "Post"         { POST }
          | "Inv"          { INV }
          | "malloc"	     {MALLOC}
          | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
	        | ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']* as lxm { VAR(lxm) }
          | eof            { EOF }
