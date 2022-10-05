grammar sos:core:concreteDefs:abstractSyntax;


--For one location display
import silver:langutil only unparse;


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

  --We want to catch if there are multiple definitions of a var
  --Sort it by var name to get all the same var together
  local sorted::[(String, QName, Type, Location)] =
     sortBy(\ p1::(String, QName, Type, Location)
              p2::(String, QName, Type, Location) ->
              p1.1 < p2.1,
            t.productionElements);
  --Group it by name to get all the same var together
  local grouped::[[(String, QName, Type, Location)]] =
     groupBy(\ p1::(String, QName, Type, Location)
               p2::(String, QName, Type, Location) ->
               p1.1 == p2.1,
             sorted);
  top.errors <-
      foldr(\ l::[(String, QName, Type, Location)] rest::[Message] ->
              if length(l) <= 1
              then rest
              else errorMessage("Multiple definitions of variable " ++
                      head(l).1 ++ " in concrete production; " ++
                      "types given are " ++
                      implode(", ",
                         map(\ p::(String, QName, Type, Location) ->
                               p.2.pp ++ " at " ++ p.4.unparse,
                             l)),
                      location=top.location)::rest,
            [], grouped);
}





nonterminal ProductionElement with
   pp,
   moduleName,
   concreteEnv,
   gatherProdElems,
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

  top.gatherProdElems = d1.gatherProdElems ++ d2.gatherProdElems;
}


--Name should refer to either a nonterminal or a terminal
abstract production nameProductionElement
top::ProductionElement ::= var::String n::QName
{
  top.pp = var ++ "::" ++ n.pp;

  n.concreteEnv = top.concreteEnv;

  top.errors <- n.concreteErrors;

  top.gatherProdElems =
      if n.concreteFound
      then [(var, n, n.concreteType, top.location)]
      else [(var, n, errorType(location=top.location), top.location)];
}


--Name should refer to either a nonterminal or a terminal
abstract production unnamedProductionElement
top::ProductionElement ::= n::QName
{
  top.pp = n.pp;

  n.concreteEnv = top.concreteEnv;

  top.errors <- n.concreteErrors;

  top.gatherProdElems = [];
}


abstract production emptyProductionElement
top::ProductionElement ::=
{
  top.pp = "";

  top.gatherProdElems = [];
}
