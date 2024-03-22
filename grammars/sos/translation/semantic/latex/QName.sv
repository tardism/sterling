grammar sos:translation:semantic:latex;

--string is macro name
--only have the name, not the slash
synthesized attribute latexConstr::String occurs on QName;
synthesized attribute latexRel::String occurs on QName;
--macro for projecting a particular type (QName is the type name)
synthesized attribute latexProj::String occurs on QName;

aspect production baseName
top::QName ::= name::String
{
  top.latexConstr = constrMacro(top.fullConstrName);
  top.latexRel = relMacro(top.fullJudgment.name);
  --We know the type is a named type, so the pp is the full name
  top.latexProj = projMacro(baseName(top.fullTy.pp,
                                     location=top.location));
}


aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  top.latexConstr = constrMacro(top.fullConstrName);
  top.latexRel = relMacro(top.fullJudgment.name);
  --We know the type is a named type, so the pp is the full name
  top.latexProj = projMacro(baseName(top.fullTy.pp,
                                     location=top.location));
}




--Build the macro names from fully-qualified names
function constrMacro
String ::= fullName::QName
{
  return "constrQQ" ++ qualifiedStringToLaTeXMacro(fullName.pp);
}
function relMacro
String ::= fullName::QName
{
  return "relQQ" ++ qualifiedStringToLaTeXMacro(fullName.pp);
}
function projMacro
String ::= fullName::QName
{
  return "projQQ" ++ qualifiedStringToLaTeXMacro(fullName.pp);
}


function qualifiedStringToLaTeXMacro
String ::= qstring::String
{
  --get rid of colons and underscores
  return substitute("_", "Z", substitute(":", "X", qstring));
}
