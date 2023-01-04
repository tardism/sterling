grammar sos:translation:semantic:extensibella;

synthesized attribute eb_base::String occurs on QName;

synthesized attribute ebTypeName::String occurs on QName;
synthesized attribute ebConstructorName::String occurs on QName;
synthesized attribute ebJudgmentName::String occurs on QName;
synthesized attribute ebTranslationName::String occurs on QName;

synthesized attribute ebUnknownName::String occurs on QName;

aspect production baseName
top::QName ::= name::String
{
  top.eb_base = name;

  top.ebTypeName =
      error("Must have full type name for translation (" ++
            top.pp ++ ") (" ++ top.location.filename ++ ":" ++
            toString(top.location.line) ++ ":" ++
            toString(top.location.column) ++ ")");
  top.ebConstructorName =
      if startsWith("$unknown__", name)
      then name
      else error("Must have full constructor name for translation" ++
                 " (" ++ top.pp ++ ")");
  top.ebJudgmentName = top.fullJudgment.eb;
  top.ebTranslationName =
      error("Must have full type name for translation of " ++
            "translation (" ++ top.pp ++ ") (" ++
            top.location.filename ++ ":" ++
            toString(top.location.line) ++ ":" ++
            toString(top.location.column) ++ ")");

  top.ebUnknownName =
      error("Must have full type for unknown name (" ++ top.pp ++
            ")");
}


aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  top.eb_base = name ++ name_sep ++ rest.eb_base;

  top.ebJudgmentName = top.fullJudgment.eb;

  --Assume this is fully qualified, so no need to look things up
  top.ebTypeName = "$ty__" ++ top.eb_base;
  top.ebConstructorName = top.eb_base;
  top.ebTranslationName = "$trans__" ++ top.eb_base;

  top.ebUnknownName = "$unknown__" ++ top.eb_base;
}





attribute
   eb<String> --full relation name
occurs on JudgmentEnvItem;

aspect production extJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList pcIndex::Integer
{
  top.eb = "$ext__" ++ toString(pcIndex) ++ "__" ++ name.eb_base;
}


aspect production fixedJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList
{
  top.eb = "$fix__" ++ name.eb_base;
}


aspect production errorJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList
{
  top.eb = error("Should not translate in the presence of errors");
}



global name_sep::String = "-$-";
