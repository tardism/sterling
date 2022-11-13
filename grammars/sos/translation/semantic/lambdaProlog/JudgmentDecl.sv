grammar sos:translation:semantic:lambdaProlog;


attribute lpDecls occurs on JudgmentDecl;

aspect production extJudgmentDecl
top::JudgmentDecl ::= name::String ty::TypeList
{
  top.lpDecls = [typeDeclaration(fullName.lpJudgmentName,
                                 foldr(arrowLPType(_, _),
                                       oLPType(), ty.types.lp))];
}


aspect production fixedJudgmentDecl
top::JudgmentDecl ::= name::String ty::TypeList
{
  top.lpDecls = [typeDeclaration(fullName.lpJudgmentName,
                                 foldr(arrowLPType(_, _),
                                       oLPType(), ty.types.lp))];
}


aspect production errorJudgmentDecl
top::JudgmentDecl ::= errs::[Message] name::String ty::TypeList
{
  top.lpDecls = error("Should not translate in presence of errors");
}





aspect production translationTypeDecl
top::JudgmentDecl ::= tyname::String args::TypeList
{
  local transTy::LambdaPrologType = nameLPType(fullTyName.lpTypeName);
  top.lpDecls =
      [typeDeclaration(fullTyName.lpTranslationName,
          foldr(arrowLPType, oLPType(),
                args.types.lp ++ [transTy, transTy]))];
}


aspect production errorTranslationDecl
top::JudgmentDecl ::= errs::[Message] tyname::String args::TypeList
{
  top.lpDecls = error("Should not translate in presence of errors");
}

