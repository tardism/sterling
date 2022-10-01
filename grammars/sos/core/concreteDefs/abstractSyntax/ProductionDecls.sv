grammar sos:core:concreteDefs:abstractSyntax;

nonterminal ConcreteSyntaxDecl with
   pp,
   moduleName,
   tyEnv, constructorEnv, concreteEnv,
   concreteDecls,
   errors,
   location;
propagate errors on ConcreteSyntaxDecl;

abstract production newConcreteNonterminal
top::ConcreteSyntaxDecl ::= name::String ty::Type d::ConcreteProdDecls
{
  top.pp = name ++ " <" ++ ty.pp ++ "> ::= " ++ d.pp;

  local fullName::QName = addQNameBase(top.moduleName, name);

  d.moduleName = top.moduleName;

  d.tyEnv = top.tyEnv;
  ty.tyEnv = top.tyEnv;
  d.constructorEnv = top.constructorEnv;
  d.concreteEnv = top.concreteEnv;

  top.concreteDecls = [concreteNT(fullName, ty)];

  --Check there is only one declaration of this name
  local possibleConcretes::[ConcreteEnvItem] =
        lookupEnv(fullName, top.concreteEnv);
  top.errors <-
      case possibleConcretes of
      | [] -> error("Impossible:  Terminal " ++ fullName.pp ++
                    " must exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for concrete " ++
            "name " ++ fullName.pp, location=top.location)]
      end;
}


abstract production addConcreteNonterminal
top::ConcreteSyntaxDecl ::= name::QName d::ConcreteProdDecls
{
  top.pp = name.pp ++ " ::= ... | " ++ d.pp;

  d.moduleName = top.moduleName;

  d.tyEnv = top.tyEnv;
  d.constructorEnv = top.constructorEnv;
  name.concreteEnv = top.concreteEnv;
  d.concreteEnv = top.concreteEnv;

  top.concreteDecls = [];

  top.errors <- name.concreteErrors;
  top.errors <-
      if !name.concreteFound
      then []
      else if name.isConcreteNt
      then []
      else [errorMessage(name.pp ++ " is not a concrete " ++
               "nonterminal and cannot have concrete productions",
               location=top.location)];
}





nonterminal ConcreteProdDecls with
   pp,
   moduleName,
   tyEnv, constructorEnv, concreteEnv,
   errors,
   location;
propagate errors on ConcreteProdDecls;

abstract production branchConcreteProdDecls
top::ConcreteProdDecls ::= d1::ConcreteProdDecls d2::ConcreteProdDecls
{
  top.pp = d1.pp ++ " | " ++ d2.pp;

  d1.moduleName = top.moduleName;
  d2.moduleName = top.moduleName;

  d1.tyEnv = top.tyEnv;
  d2.tyEnv = top.tyEnv;
  d1.constructorEnv = top.constructorEnv;
  d2.constructorEnv = top.constructorEnv;
  d1.concreteEnv = top.concreteEnv;
  d2.concreteEnv = top.concreteEnv;
}


abstract production concreteProdDecl
top::ConcreteProdDecls ::= p::ProductionElement t::Term
{
  top.pp = p.pp ++ " ~~> { " ++ t.pp ++ " }";

  p.moduleName = top.moduleName;

  t.tyEnv = top.tyEnv;
  t.constructorEnv = top.constructorEnv;

  p.concreteEnv = top.concreteEnv;

  t.productionElements = p.gatherProdElems;
}





nonterminal ProductionElement with
   pp,
   moduleName,
   concreteEnv,
   gatherProdElems,
   typeList,
   errors,
   location;
propagate errors on ProductionElement;

abstract production branchProductionElement
top::ProductionElement ::= d1::ProductionElement d2::ProductionElement
{
  top.pp = d1.pp ++ " " ++ d2.pp;

  d1.moduleName = top.moduleName;
  d2.moduleName = top.moduleName;

  d1.concreteEnv = top.concreteEnv;
  d2.concreteEnv = top.concreteEnv;

  top.typeList = d1.typeList ++ d2.typeList;

  top.gatherProdElems = d1.gatherProdElems ++ d2.gatherProdElems;
}


--Name should refer to either a nonterminal or a terminal
abstract production nameProductionElement
top::ProductionElement ::= n::QName
{
  top.pp = n.pp;

  n.concreteEnv = top.concreteEnv;

  top.errors <- n.concreteErrors;

  top.typeList =
      if n.concreteFound
      then [n.concreteType]
      else [errorType(location=top.location)];

  top.gatherProdElems =
      if n.concreteFound
      then [(n, n.concreteType)]
      else [(n, errorType(location=top.location))];
}


abstract production emptyProductionElement
top::ProductionElement ::=
{
  top.pp = "";

  top.typeList = [];

  top.gatherProdElems = [];
}
