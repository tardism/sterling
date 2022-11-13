grammar sos:translation:semantic:lambdaProlog;


attribute lp<String> occurs on QName;

flowtype lp {} on QName;

synthesized attribute lpTypeName::String occurs on QName;
synthesized attribute lpConstructorName::String occurs on QName;
synthesized attribute lpJudgmentName::String occurs on QName;
synthesized attribute lpTranslationName::String occurs on QName;

aspect production baseName
top::QName ::= name::String
{
  top.lp = name;

  top.lpTypeName = "ty_**_" ++ top.lp;
  top.lpConstructorName = "constr_**_" ++ top.lp;
  top.lpJudgmentName = "jdg_**_" ++ top.lp;
  top.lpTranslationName = "trans_**_" ++ top.lp;
}


aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  top.lp = name ++ "-" ++ rest.lp;

  top.lpTypeName = "ty_**_" ++ top.lp;
  top.lpConstructorName = "constr_**_" ++ top.lp;
  top.lpJudgmentName = "jdg_**_" ++ top.lp;
  top.lpTranslationName = "trans_**_" ++ top.lp;
}
