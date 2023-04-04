grammar sos:testing:mainConcreteSyntax;

imports sos:core:common:concreteSyntax;
imports sos:core:main:concreteSyntax;
imports sos:core:main:abstractSyntax;
imports sos:testing:abstractSyntax;

terminal Error_t      'Error'      lexer classes {KEYWORD};
terminal Warning_t    'Warning'    lexer classes {KEYWORD};
terminal Expected_t   'Expected'   lexer classes {KEYWORD};

terminal String_t    /"([^"]|(\\"))*"/;

concrete productions top::MainDeclList_c
| 'Error' EmptyNewlines 'Expected' EmptyNewlines s::String_t
  EmptyNewlines '{' EmptyNewlines decls::MainDeclList_c '}'
  EmptyNewlines rest::MainDeclList_c
  { top.ast = branchMainDecls(
                 errorExpectedMainDecls(
                    substring(1, length(s.lexeme) - 1, s.lexeme),
                    decls.ast, location=top.location),
                 rest.ast, location=top.location); }
| 'Warning' EmptyNewlines 'Expected' EmptyNewlines s::String_t
  EmptyNewlines '{' EmptyNewlines decls::MainDeclList_c '}'
  EmptyNewlines rest::MainDeclList_c
  { top.ast = branchMainDecls(
                 warningExpectedMainDecls(
                    substring(1, length(s.lexeme) - 1, s.lexeme),
                    decls.ast, location=top.location),
                 rest.ast, location=top.location); }
