grammar sos:core:concreteDefs:abstractSyntax;

nonterminal ConcreteFile with
   pp,
   location;

abstract production concreteFile
top::ConcreteFile ::= moduleName::QName decls::ConcreteDecls
{

}

