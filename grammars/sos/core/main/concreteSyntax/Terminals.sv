grammar sos:core:main:concreteSyntax;

imports sos:core:common:concreteSyntax;
imports sos:core:common:abstractSyntax;

imports sos:core:semanticDefs:concreteSyntax;

imports sos:core:main:abstractSyntax;


terminal Parse_t   'Parse'   lexer classes {KEYWORD};
terminal As_t      'as'      lexer classes {KEYWORD};
terminal From_t    'from'    lexer classes {KEYWORD};

terminal Assign_t   ':=';
