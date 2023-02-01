grammar sos:core:main:concreteSyntax;

imports sos:core:common:concreteSyntax;
imports sos:core:common:abstractSyntax;

imports sos:core:semanticDefs:concreteSyntax;

imports sos:core:main:abstractSyntax;


terminal Parse_t   'Parse'   lexer classes {KEYWORD};
terminal As_t      'as'      lexer classes {KEYWORD};
terminal From_t    'from'    lexer classes {KEYWORD};

terminal Derive_t      'Derive'      lexer classes {KEYWORD};
terminal Assigning_t   'assigning'   lexer classes {KEYWORD};
terminal For_t         'for'         lexer classes {KEYWORD};

terminal Fun_t      'Function'   lexer classes {KEYWORD};
terminal Arrow_t    '->';
terminal LCurly_t   '{';
terminal RCurly_t   '}';

terminal If_t       'If'       lexer classes {KEYWORD};
terminal Then_t     'Then'     lexer classes {KEYWORD};
terminal Else_t     'Else'     lexer classes {KEYWORD};
terminal Let_t      'Let'      lexer classes {KEYWORD};
terminal In_t       'in'       lexer classes {KEYWORD};
terminal Before_t   'Before'   lexer classes {KEYWORD};

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
