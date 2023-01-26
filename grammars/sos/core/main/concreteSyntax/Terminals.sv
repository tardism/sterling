grammar sos:core:main:concreteSyntax;

imports sos:core:common:concreteSyntax;
imports sos:core:common:abstractSyntax;

imports sos:core:semanticDefs:concreteSyntax;

imports sos:core:main:abstractSyntax;


terminal Parse_t   'Parse'   lexer classes {KEYWORD};
terminal As_t      'as'      lexer classes {KEYWORD};
terminal From_t    'from'    lexer classes {KEYWORD};

terminal Derive_t      'Derive'      lexer classes {KEYWORD};
terminal Assigning_t   'Assigning'   lexer classes {KEYWORD};

terminal Fun_t      'Function'   lexer classes {KEYWORD};
terminal Arrow_t    '->';
terminal LCurly_t   '{';
terminal RCurly_t   '}';
terminal Return_t   'Return'     lexer classes {KEYWORD};

terminal If_t      'If'      lexer classes {KEYWORD};
terminal Then_t    'Then'    lexer classes {KEYWORD};
terminal Else_t    'Else'    lexer classes {KEYWORD};
terminal While_t   'While'   lexer classes {KEYWORD};
terminal Do_t      'Do'      lexer classes {KEYWORD};
terminal End_t     'End'     lexer classes {KEYWORD};

terminal Print_t   'Print'   lexer classes {KEYWORD};
terminal Read_t    'Read'    lexer classes {KEYWORD};
terminal Write_t   'Write'   lexer classes {KEYWORD};
terminal To_t      'to'      lexer classes {KEYWORD};

terminal Assign_t   ':=';

terminal Or_t    '||';
terminal And_t   '&&';

terminal Bool_t    'bool'    lexer classes {KEYWORD};
terminal True_t    'true'    lexer classes {KEYWORD};
terminal False_t   'false'   lexer classes {KEYWORD};
