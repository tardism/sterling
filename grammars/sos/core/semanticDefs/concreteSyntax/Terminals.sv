grammar sos:core:semanticDefs:concreteSyntax;

imports sos:core:common:concreteSyntax;
imports sos:core:common:abstractSyntax;

imports sos:core:semanticDefs:abstractSyntax;


terminal Builds_t       'Builds'        lexer classes {KEYWORD};
terminal On_t           'on'            lexer classes {KEYWORD};
terminal Jdgmt_t        'Judgment'      lexer classes {KEYWORD};
terminal Fixed_t        'Fixed'         lexer classes {KEYWORD};
terminal Project_t      'Projection'    lexer classes {KEYWORD};


--Line for a rule is a minimum of three dashes/equals signs
terminal ExtLine_t        /---+/ dominates {Minus_t};
terminal FixedLine_t      /===+/;


terminal Eq_t         '=';
terminal Neq_t        '!=';
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

terminal Negate_t   '!';

terminal Cons_t     '::'   association=right;
terminal LCurly_t   '{';
terminal RCurly_t   '}';

terminal ProjStart_t    '|{';
terminal ProjEnd_t      '}-';
terminal Proj_t         '~~>';

terminal Upright_t    '|';
terminal ColonsEq_t   '::=';

terminal Integer_t   /-?[0-9]+/;
terminal String_t    /"([^"]|(\\"))*"/;


terminal UpperId_t      /[A-Z][a-z0-9A-Z_]*/ submits to {KEYWORD};
terminal RuleName_t     /[a-zA-Z][-a-z0-9A-Z_]*/;

