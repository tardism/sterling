grammar sos:core:common:abstractSyntax;


nonterminal QName with
   pp,
   tyEnv, constructorEnv,
   location;


--Easy equality checking, ordering
attribute compareTo, isEqual occurs on QName;

instance Ord QName{
  compare = \ a::QName b::QName -> silver:core:compare(a.pp, b.pp);
}


--Get the short name in the qualified name
synthesized attribute base::String occurs on QName;

--Get the module for the qualified name ("" for non-qualified)
synthesized attribute baselessName::String occurs on QName;

--Put a new base name on the end (e.g. turn a:b:c into a:b:c:d)
inherited attribute addBase::String occurs on QName;
synthesized attribute baseAdded::QName occurs on QName;

synthesized attribute isQualified::Boolean occurs on QName;

--Specific errors and types, depending on what we expect a name to be
synthesized attribute tyErrors::[Message] occurs on QName;
synthesized attribute tyFound::Boolean occurs on QName;
synthesized attribute fullTy::Type occurs on QName;
--
synthesized attribute constrErrors::[Message] occurs on QName;
synthesized attribute constrType::Type occurs on QName;
synthesized attribute constrTypeArgs::TypeList occurs on QName;
synthesized attribute constrFound::Boolean occurs on QName;
synthesized attribute fullConstrName::QName occurs on QName;

abstract production baseName
top::QName ::= name::String
{
  top.pp = name;

  top.base = name;

  top.baselessName = "";

  top.baseAdded =
      moduleLayerName(name,
         baseName(top.addBase, location=top.location),
         location=top.location);

  top.isQualified = false;

  production attribute possibleTys::[TypeEnvItem];
  possibleTys = lookupEnv(^top, top.tyEnv);
  top.tyErrors =
      case possibleTys of
      | [] -> [errorMessage("Unknown type " ++ top.pp,
                            location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Indeterminate type " ++ top.pp ++ "; " ++
            "possibilities are " ++
            implode(", ", map((.pp), map((.name), l))),
            location=top.location)]
      end;
  top.tyFound = length(possibleTys) == 1;
  top.fullTy =
      if top.tyFound
      then nameType(head(possibleTys).name, location=top.location)
      else errorType(location=top.location);

  production attribute possibleCons::[ConstructorEnvItem];
  possibleCons = lookupEnv(^top, top.constructorEnv);
  top.constrErrors =
      case possibleCons of
      | [] -> [errorMessage("Unknown constructor " ++ top.pp,
                            location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Indeterminate constructor " ++
            top.pp ++ "; possibilities are " ++
            implode(", ", map((.pp), map((.name), l))),
            location=top.location)]
      end;
  top.constrType = head(possibleCons).type;
  top.constrTypeArgs = head(possibleCons).types;
  top.constrFound = length(possibleCons) == 1;
  top.fullConstrName = head(possibleCons).name;

  top.isEqual = top.compareTo.base == name;
}


abstract production moduleLayerName
top::QName ::= name::String rest::QName
{
  top.pp = name ++ ":" ++ rest.pp;

  top.base = rest.base;

  top.baselessName = name ++ if rest.baselessName == ""
                             then "" else ":" ++ rest.baselessName;

  rest.addBase = top.addBase;
  top.baseAdded =
      moduleLayerName(name, rest.baseAdded, location=top.location);

  top.isQualified = true;

  production attribute possibleTys::[TypeEnvItem];
  possibleTys = lookupEnv(^top, top.tyEnv);
  top.tyErrors =
      case possibleTys of
      | [] -> [errorMessage("Unknown type " ++ top.pp,
                            location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Indeterminate type " ++ top.pp ++ "; " ++
            "possibilities are " ++
            implode(", ", map((.pp), map((.name), l))),
            location=top.location)]
      end;
  top.tyFound = length(possibleTys) == 1;
  top.fullTy =
      if top.tyFound
      then nameType(head(possibleTys).name, location=top.location)
      else errorType(location=top.location);

  production attribute possibleCons::[ConstructorEnvItem];
  possibleCons = lookupEnv(^top, top.constructorEnv);
  top.constrErrors =
      case possibleCons of
      | [] -> [errorMessage("Unknown constructor " ++ top.pp,
                            location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Indeterminate constructor " ++
            top.pp ++ "; possibilities are " ++
            implode(", ", map((.pp), map((.name), l))),
            location=top.location)]
      end;
  top.constrType = head(possibleCons).type;
  top.constrTypeArgs = head(possibleCons).types;
  top.constrFound = length(possibleCons) == 1;
  top.fullConstrName = head(possibleCons).name;

  rest.compareTo =
       case top.compareTo of
       | moduleLayerName(n, r) -> r
       | x -> x
       end;
  top.isEqual =
      rest.isEqual &&
      case top.compareTo of
      | moduleLayerName(n, r) -> n == name
      | _ -> true
      end;
}





function toQName
QName ::= name::String loc::Location
{
  return
     foldrLastElem(moduleLayerName(_, _, location=loc),
                   baseName(_, location=loc), explode(":", name));
}


function addQNameBase
QName ::= module::QName name::String
{
  module.addBase = name;
  return module.baseAdded;
}


--identifier must be fully-qualified
function sameModule
Boolean ::= moduleName::QName identifier::QName
{
  return if !identifier.isQualified
         then error("Identifier must be qualified (" ++
                    identifier.pp ++ ")")
         else addQNameBase(^moduleName, identifier.base) == ^identifier;
}

