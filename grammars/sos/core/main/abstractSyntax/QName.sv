grammar sos:core:main:abstractSyntax;

attribute
   funEnv
occurs on QName;

--Specific errors and types for functions
synthesized attribute functionErrors::[Message] occurs on QName;
synthesized attribute functionArgTypes::TypeList occurs on QName;
synthesized attribute functionRetType::Type occurs on QName;
synthesized attribute functionFound::Boolean occurs on QName;
synthesized attribute fullFunction::FunctionEnvItem occurs on QName;

aspect production baseName
top::QName ::= name::String
{
  production attribute possibleFunctions::[FunctionEnvItem];
  possibleFunctions = lookupEnv(top, top.funEnv);
  top.functionErrors =
      case possibleFunctions of
      | [] -> [errorMessage("Unknown function " ++ top.pp,
                            location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Indeterminate function " ++ top.pp ++
            "; possibilities are " ++
            implode(", ", map((.pp), map((.name), l))),
            location=top.location)]
      end;
  top.functionArgTypes = head(possibleFunctions).types;
  top.functionRetType = head(possibleFunctions).type;
  top.functionFound = length(possibleFunctions) == 1;
  top.fullFunction =
      if top.functionFound
      then head(possibleFunctions)
      else errorFunctionEnvItem(top);
}


aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  production attribute possibleFunctions::[FunctionEnvItem];
  possibleFunctions = lookupEnv(top, top.funEnv);
  top.functionErrors =
      case possibleFunctions of
      | [] -> [errorMessage("Unknown function " ++ top.pp,
                            location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Indeterminate function " ++ top.pp ++
            "; possibilities are " ++
            implode(", ", map((.pp), map((.name), l))),
            location=top.location)]
      end;
  top.functionArgTypes = head(possibleFunctions).types;
  top.functionRetType = head(possibleFunctions).type;
  top.functionFound = length(possibleFunctions) == 1;
  top.fullFunction =
      if top.functionFound
      then head(possibleFunctions)
      else errorFunctionEnvItem(top);
}
