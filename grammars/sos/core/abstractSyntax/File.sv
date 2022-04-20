grammar sos:core:abstractSyntax;


nonterminal File with pp, errors, location;
propagate errors on File;

abstract production file
top::File ::= moduleName::QName decls::Decls
{
  top.pp = "Module " ++ moduleName.pp ++ "\n\n" ++ decls.pp;

  decls.moduleName = moduleName;

  decls.tyEnv = map(\ t::TypeEnvItem -> (t.name, t), decls.tyDecls);
  decls.constructorEnv =
        map(\ c::ConstructorEnvItem -> (c.name, c),
            decls.constructorDecls);
  decls.judgmentEnv =
        map(\ j::JudgmentEnvItem -> (j.name, j),
            decls.judgmentDecls);
  decls.translationEnv =
        map(\ t::TranslationEnvItem -> (t.name, t),
            decls.translationDecls);
}





nonterminal Decls with
   pp,
   moduleName,
   tyDecls, constructorDecls, judgmentDecls, translationDecls,
   tyEnv, constructorEnv, judgmentEnv, translationEnv,
   errors,
   location;
propagate errors on Decls;

abstract production nilDecls
top::Decls ::=
{
  top.pp = "";

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.translationDecls = [];
}


abstract production ruleDecls
top::Decls ::= r::Rule
{
  top.pp = r.pp;

  r.moduleName = top.moduleName;

  top.tyDecls = r.tyDecls;
  top.constructorDecls = r.constructorDecls;
  top.judgmentDecls = r.judgmentDecls;
  top.translationDecls = r.translationDecls;

  r.tyEnv = top.tyEnv;
  r.constructorEnv = top.constructorEnv;
  r.judgmentEnv = top.judgmentEnv;
  r.translationEnv = top.translationEnv;
}


abstract production absSyntaxDecls
top::Decls ::= a::AbsSyntaxDecl
{
  top.pp = a.pp;

  a.moduleName = top.moduleName;

  top.tyDecls = a.tyDecls;
  top.constructorDecls = a.constructorDecls;
  top.judgmentDecls = a.judgmentDecls;
  top.translationDecls = a.translationDecls;

  a.tyEnv = top.tyEnv;
  a.constructorEnv = top.constructorEnv;
  a.judgmentEnv = top.judgmentEnv;
  a.translationEnv = top.translationEnv;
}


abstract production judgmentDecls
top::Decls ::= j::JudgmentDecl
{
  top.pp = j.pp;

  j.moduleName = top.moduleName;

  top.tyDecls = j.tyDecls;
  top.constructorDecls = j.constructorDecls;
  top.judgmentDecls = j.judgmentDecls;
  top.translationDecls = j.translationDecls;

  j.tyEnv = top.tyEnv;
  j.constructorEnv = top.constructorEnv;
  j.judgmentEnv = top.judgmentEnv;
  j.translationEnv = top.translationEnv;
}


abstract production branchDecls
top::Decls ::= d1::Decls d2::Decls
{
  top.pp = d1.pp ++ "\n" ++ d2.pp;

  d1.moduleName = top.moduleName;
  d2.moduleName = top.moduleName;

  top.tyDecls = d1.tyDecls ++ d2.tyDecls;
  top.constructorDecls = d1.constructorDecls ++ d2.constructorDecls;
  top.judgmentDecls = d1.judgmentDecls ++ d2.judgmentDecls;
  top.translationDecls = d1.translationDecls ++ d2.translationDecls;

  d1.tyEnv = top.tyEnv;
  d1.constructorEnv = top.constructorEnv;
  d1.judgmentEnv = top.judgmentEnv;
  d1.translationEnv = top.translationEnv;
  d2.tyEnv = top.tyEnv;
  d2.constructorEnv = top.constructorEnv;
  d2.judgmentEnv = top.judgmentEnv;
  d2.translationEnv = top.translationEnv;
}

