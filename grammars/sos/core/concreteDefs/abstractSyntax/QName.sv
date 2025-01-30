grammar sos:core:concreteDefs:abstractSyntax;


attribute
   concreteEnv
occurs on QName;


synthesized attribute concreteErrors::[Message] occurs on QName;
synthesized attribute concreteFound::Boolean occurs on QName;
synthesized attribute concreteType::Type occurs on QName;
synthesized attribute fullConcreteName::QName occurs on QName;
synthesized attribute isConcreteNt::Boolean occurs on QName;

aspect production baseName
top::QName ::= name::String
{
  production attribute possibleConcretes::[ConcreteEnvItem];
  possibleConcretes = lookupEnv(^top, top.concreteEnv);
  top.concreteErrors =
      case possibleConcretes of
      | [] -> [errorMessage("Unknown terminal or concrete " ++
                  "nonterminal " ++ top.pp,
                  location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Indeterminate terminal or concrete " ++
            "nonterminal " ++ top.pp ++ "; possibilities are " ++
            implode(", ", map((.pp), map((.name), l))),
            location=top.location)]
      end;
  top.concreteFound = length(possibleConcretes) == 1;
  top.concreteType = head(possibleConcretes).type;
  top.fullConcreteName = head(possibleConcretes).name;
  top.isConcreteNt = head(possibleConcretes).isConcreteNt;
}


aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  production attribute possibleConcretes::[ConcreteEnvItem];
  possibleConcretes = lookupEnv(^top, top.concreteEnv);
  top.concreteErrors =
      case possibleConcretes of
      | [] -> [errorMessage("Unknown terminal or concrete " ++
                  "nonterminal " ++ top.pp,
                  location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Indeterminate terminal or concrete " ++
            "nonterminal " ++ top.pp ++ "; possibilities are " ++
            implode(", ", map((.pp), map((.name), l))),
            location=top.location)]
      end;
  top.concreteFound = length(possibleConcretes) == 1;
  top.concreteType = head(possibleConcretes).type;
  top.fullConcreteName = head(possibleConcretes).name;
  top.isConcreteNt = head(possibleConcretes).isConcreteNt;
}
