grammar sos:core:main:concreteSyntax;

imports sos:core:common:concreteSyntax;
imports sos:core:common:abstractSyntax;

imports sos:core:semanticDefs:concreteSyntax;

imports sos:core:main:abstractSyntax;


terminal Parse_t   'Parse'   lexer classes {KEYWORD};
terminal As_t      'as'      lexer classes {KEYWORD};
terminal From_t    'from'    lexer classes {KEYWORD};

terminal Fun_t   'Function'   lexer classes {KEYWORD};

terminal Print_t   'Print'   lexer classes {KEYWORD};
terminal Read_t    'Read'    lexer classes {KEYWORD};
terminal Write_t   'Write'   lexer classes {KEYWORD};
terminal To_t      'to'      lexer classes {KEYWORD};

terminal Assign_t   ':=';

terminal Bool_t      '?result';
terminal Success_t   '?success';
terminal Fail_t      '?fail';
