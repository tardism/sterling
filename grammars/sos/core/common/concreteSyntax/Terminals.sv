grammar sos:core:common:concreteSyntax;

imports sos:core:common:abstractSyntax;


lexer class KEYWORD dominates {LowerId_t};


terminal Module_t       'Module'        lexer classes {KEYWORD};


terminal StringTy_t   'string'   lexer classes {KEYWORD};
terminal IntTy_t      'int'      lexer classes {KEYWORD};


--We don't need the \r in this to make it work, since \r is part of
--the ignore terminal for whitespace.  However, we get a warning that
--we have \n but not \r here without it.
terminal Newline_t        /\r?\n/;


terminal LowerId_t      /[a-z][a-z0-9A-Z_]*/;
terminal LowerQName_t   /([a-zA-Z_]+:)+[a-z][a-z0-9A-Z_]*/;


ignore terminal Spacing_t   /[\ \t\r]+/;
ignore terminal Comment_t   /\/\*(\/\*([^\*]|\*+[^\/\*])*\*+\/|[^\*]|\*+[^\/\*])*\*+\//;

