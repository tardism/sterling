grammar sos:core:files:concreteSyntax;


imports sos:core:files:abstractSyntax;


lexer class KEYWORD dominates {UpperId_t, LowerId_t};


terminal Module_t       'Module'        lexer classes {KEYWORD};
terminal Builds_t       'Builds'        lexer classes {KEYWORD};
terminal On_t           'on'            lexer classes {KEYWORD};
terminal Jdgmt_t        'Judgment'      lexer classes {KEYWORD};
terminal Fixed_t        'Fixed'         lexer classes {KEYWORD};
terminal Translate_t    'Translation'   lexer classes {KEYWORD};


terminal StringTy_t   'string'   lexer classes {KEYWORD};
terminal IntTy_t      'int'      lexer classes {KEYWORD};


--Line for a rule is a minimum of three dashes/equals signs
terminal ExtLine_t        /---+/ dominates {Minus_t};
terminal FixedLine_t      /===+/;
--We don't need the \r in this to make it work, since \r is part of
--the ignore terminal for whitespace.  However, we get a warning that
--we have \n but not \r here without it.
terminal Newline_t        /\r?\n/;


terminal LBracket_t   '[';
terminal RBracket_t   ']';
terminal LParen_t     '(';
terminal RParen_t     ')';
terminal Eq_t         '=';
terminal Neq_t        '!=';
terminal Comma_t      ',';
terminal Colon_t      ':';
terminal Period_t     '.';
terminal LAngle_t     '<';
terminal RAngle_t     '>';
terminal Geq_t        '>=';
terminal Leq_t        '<=';

terminal Plus_t    '+';
terminal Minus_t   '-';
terminal Mult_t    '*';
terminal Div_t     '/';
terminal Mod_t     '%';
terminal App_t     '++';

terminal TransStart_t    '|{';
terminal TransEnd_t      '}-';
terminal Trans_t         '~~>';

terminal Upright_t    '|';
terminal ColonsEq_t   '::=';

terminal Integer_t   /[0-9]+/;
terminal String_t    /"([^"]|(\\"))*"/;


terminal LowerId_t      /[a-z][a-z0-9A-Z_]*/;
terminal UpperId_t      /[A-Z][a-z0-9A-Z_]*/;
terminal LowerQName_t   /([a-zA-Z_]+:)+[a-z][a-z0-9A-Z_]*/;
terminal UpperQName_t   /([a-zA-Z_]+:)+[A-Z][a-z0-9A-Z_]*/;
terminal RuleName_t     /[a-zA-Z][-a-z0-9A-Z_]*/;


ignore terminal Spacing_t   /[\ \t\r]+/;
ignore terminal Comment_t   /#[^#]*#/;

