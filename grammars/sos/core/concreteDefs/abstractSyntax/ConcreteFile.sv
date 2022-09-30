grammar sos:core:concreteDefs:abstractSyntax;

nonterminal ConcreteFile with
   pp,
   moduleName,
   concreteDecls,
   tyEnv, constructorEnv, concreteEnv,
   errors,
   location;
propagate errors on ConcreteFile;

abstract production concreteFile
top::ConcreteFile ::= moduleName::QName decls::ConcreteDecls
{
  top.pp = "Module " ++ moduleName.pp ++ "\n\n" ++ decls.pp;

  decls.moduleName = moduleName;

  top.concreteDecls = decls.concreteDecls;

  decls.tyEnv = top.tyEnv;
  decls.constructorEnv = top.constructorEnv;
  decls.concreteEnv = top.concreteEnv;
}





nonterminal ConcreteDecls with
   pp,
   moduleName,
   tyEnv, constructorEnv, concreteEnv,
   concreteDecls,
   errors,
   location;
propagate errors on ConcreteDecls;

abstract production nilDecls
top::ConcreteDecls ::=
{
  top.pp = "";

  top.concreteDecls = [];
}


abstract production terminalDecl
top::ConcreteDecls ::= d::TerminalDecl
{
  top.pp = d.pp;

  d.moduleName = top.moduleName;

  top.concreteDecls = d.concreteDecls;

  d.concreteEnv = top.concreteEnv;
}


abstract production concreteSyntaxDecl
top::ConcreteDecls ::= d::ConcreteSyntaxDecl
{
  top.pp = d.pp;

  d.moduleName = top.moduleName;

  d.tyEnv = top.tyEnv;
  d.constructorEnv = top.constructorEnv;
  d.concreteEnv = top.concreteEnv;

  top.concreteDecls = d.concreteDecls;
}


abstract production branchConcreteDecls
top::ConcreteDecls ::= d1::ConcreteDecls d2::ConcreteDecls
{
  top.pp = d1.pp ++ "\n" ++ d2.pp;

  d1.moduleName = top.moduleName;
  d2.moduleName = top.moduleName;

  d1.tyEnv = top.tyEnv;
  d2.tyEnv = top.tyEnv;
  d1.constructorEnv = top.constructorEnv;
  d2.constructorEnv = top.constructorEnv;
  d1.concreteEnv = top.concreteEnv;
  d2.concreteEnv = top.concreteEnv;

  top.concreteDecls = d1.concreteDecls ++ d2.concreteDecls;
}

