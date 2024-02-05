grammar sos:translation:semantic:extensibella:concreteSyntax;

imports sos:core:common:concreteSyntax;
imports sos:core:semanticDefs:concreteSyntax;

imports sos:translation:semantic:extensibella:abstractSyntax;


concrete productions top::TopDecl_c
| 'Extensibella_Stand_In' '{' r::Rule_c '}'
  { top.ast = standInRuleDecls(r.ast, location=top.location); }

terminal StandIn_t   'Extensibella_Stand_In'   lexer classes {KEYWORD};
