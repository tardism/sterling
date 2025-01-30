grammar sos:core:semanticDefs:abstractSyntax;


attribute
   judgmentEnv
occurs on QName;


--Specific errors and types for judgments
synthesized attribute judgmentErrors::[Message] occurs on QName;
synthesized attribute judgmentType::TypeList occurs on QName;
synthesized attribute judgmentFound::Boolean occurs on QName;
synthesized attribute fullJudgment::JudgmentEnvItem occurs on QName;

aspect production baseName
top::QName ::= name::String
{
  production attribute possibleJudgments::[JudgmentEnvItem];
  possibleJudgments = lookupEnv(^top, top.judgmentEnv);
  top.judgmentErrors =
      case possibleJudgments of
      | [] -> [errorMessage("Unknown judgment " ++ top.pp,
                            location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Indeterminate judgment " ++
            top.pp ++ "; possibilities are " ++
            implode(", ", map((.pp), map((.name), l))),
            location=top.location)]
      end;
  top.judgmentType = head(possibleJudgments).types;
  top.judgmentFound = length(possibleJudgments) == 1;
  top.fullJudgment =
      if top.judgmentFound
      then head(possibleJudgments)
      else errorJudgmentEnvItem(^top,
              nilTypeList(location=top.location));
}


aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  production attribute possibleJudgments::[JudgmentEnvItem];
  possibleJudgments = lookupEnv(^top, top.judgmentEnv);
  top.judgmentErrors =
      case possibleJudgments of
      | [] -> [errorMessage("Unknown judgment " ++ top.pp,
                            location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Indeterminate judgment " ++
            top.pp ++ "; possibilities are " ++
            implode(", ", map((.pp), map((.name), l))),
            location=top.location)]
      end;
  top.judgmentType = head(possibleJudgments).types;
  top.judgmentFound = length(possibleJudgments) == 1;
  top.fullJudgment = head(possibleJudgments);
}

