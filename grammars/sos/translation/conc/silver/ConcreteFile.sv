grammar sos:translation:conc:silver;

attribute
  silverConc<[SilverConcDecl]>
occurs on ConcreteFile;

aspect production concreteFile
top::ConcreteFile ::= moduleName::QName decls::ConcreteDecls
{
  top.silverConc = decls.silverConc;
}





attribute
  silverConc<[SilverConcDecl]>
occurs on ConcreteDecls;

aspect production nilDecls
top::ConcreteDecls ::=
{
  top.silverConc = [];
}


aspect production terminalDecl
top::ConcreteDecls ::= d::TerminalDecl
{
  top.silverConc = [d.silverConc];
}


aspect production concreteSyntaxDecl
top::ConcreteDecls ::= d::ConcreteSyntaxDecl
{
  top.silverConc = d.silverConc;
}


aspect production branchConcreteDecls
top::ConcreteDecls ::= d1::ConcreteDecls d2::ConcreteDecls
{
  top.silverConc = d1.silverConc ++ d2.silverConc;
}
