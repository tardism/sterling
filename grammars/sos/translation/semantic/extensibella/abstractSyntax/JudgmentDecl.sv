grammar sos:translation:semantic:extensibella:abstractSyntax;

attribute
   ebJudgments
occurs on JudgmentDecl;

aspect production extJudgmentDecl
top::JudgmentDecl ::= name::String ty::TypeList
{
  local fullNameCopy::QName = ^fullName;
  fullNameCopy.judgmentEnv = top.judgmentEnv;
  top.ebJudgments = [(fullNameCopy.ebJudgmentName, ty.eb)];
}


aspect production fixedJudgmentDecl
top::JudgmentDecl ::= name::String ty::TypeList
{
  local fullNameCopy::QName = ^fullName;
  fullNameCopy.judgmentEnv = top.judgmentEnv;
  top.ebJudgments = [(fullNameCopy.ebJudgmentName, ty.eb)];
}


aspect production errorJudgmentDecl
top::JudgmentDecl ::= errs::[Message] name::String ty::TypeList
{
  top.ebJudgments =
      error("Should not translate in the presence of errors");
}





aspect production projectionTypeDecl
top::JudgmentDecl ::= tyname::String args::TypeList
{
  local ty::ExtensibellaType =
      extensibellaNameTy(fullTyName.ebTypeName);
  top.ebJudgments =
      [(fullTyName.ebProjectionName, args.eb ++ [^ty, ^ty])];
}


aspect production errorProjectionDecl
top::JudgmentDecl ::= errs::[Message] tyname::String args::TypeList
{
  top.ebJudgments =
      error("Should not translate in the presence of errors");
}
