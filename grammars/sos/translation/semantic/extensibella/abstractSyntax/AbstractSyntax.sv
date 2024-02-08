grammar sos:translation:semantic:extensibella:abstractSyntax;


attribute
  ebKinds, ebConstrs, ebStandInRules
occurs on AbsSyntaxDecl;

aspect production initialAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::String constructors::AbsConstructorDecls
{
  top.ebKinds = [kindDecl(fullName.ebTypeName)];

  top.ebConstrs = constructors.ebConstrs;
  constructors.builtEBType = extensibellaNameTy(fullName.ebTypeName);

  local is::String = fullName.ebIsName;
  local trans::TranslationEnvItem =
      head(lookupEnv(fullName, top.translationEnv));
  local transArgs::[ExtensibellaTerm] =
      map(nameExtensibellaTerm,
          map(\ x::Integer -> "A" ++ toString(x),
              range(1, trans.types.len)) ++ ["X", "X_T"]);
  top.ebStandInRules =
      [(extJudgmentEnvItem(toQName(is, bogusLoc()),
           consTypeList(nameType(fullName, location=bogusLoc()),
                        nilTypeList(location=bogusLoc()),
                        location=bogusLoc()), 0),
        relationMetaterm(is, [nameExtensibellaTerm("X")]),
        [relationMetaterm(fullName.ebTranslationName, transArgs),
         relationMetaterm(is, [nameExtensibellaTerm("X_T")])],
        "X")];
}


aspect production addAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::QName constructors::AbsConstructorDecls
{
  --kind already declared
  top.ebKinds = [];

  top.ebConstrs = constructors.ebConstrs;
  constructors.builtEBType =
      case type.fullTy of
      | nameType(n) -> extensibellaNameTy(n.ebTypeName)
      | _ -> error("Not possible")
      end;

  top.ebStandInRules = [];
}





attribute
  ebConstrs, builtEBType
occurs on AbsConstructorDecls;

inherited attribute builtEBType::ExtensibellaType;

aspect production nilAbsConstructorDecls
top::AbsConstructorDecls ::=
{
  top.ebConstrs = [];
}


aspect production branchAbsConstructorDecls
top::AbsConstructorDecls ::= d1::AbsConstructorDecls
                             d2::AbsConstructorDecls
{
  d1.builtEBType = top.builtEBType;
  d2.builtEBType = top.builtEBType;
  top.ebConstrs = d1.ebConstrs ++ d2.ebConstrs;
}


aspect production oneConstructorDecl
top::AbsConstructorDecls ::= name::String tyargs::TypeList
{
  top.ebConstrs = [constrDecl(fullName.ebConstructorName, tyargs.eb,
                              top.builtEBType)];
}
