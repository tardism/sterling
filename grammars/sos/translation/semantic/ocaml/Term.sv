grammar sos:translation:semantic:ocaml;

attribute ocamlExpr occurs on Term;
attribute ocamlExprs occurs on TermList;

synthesized attribute decoratedTermList::[Decorated Term with {constructorEnv}] occurs on TermList;

aspect production const
top::Term ::= name::QName
{
  -- Old (module-qualified) version:
  -- top.ocamlExpr =
  --   ocamlConstructor(
  --     capitalizeFirst(if name.isQualified
  --                     then name.ocamlString
  --                     else name.fullConstrName.ocamlString),
  --     []);
  top.ocamlExpr =
    ocamlConstructor(
      capitalizeFirst(if name.isQualified
                      then name.base
                      else name.fullConstrName.base),
      []);
}

aspect production var
top::Term ::= name::String
{
  top.ocamlExpr = ocamlVar(lowercaseFirst(name));
}

aspect production num
top::Term ::= i::Integer
{
  top.ocamlExpr = ocamlInt(i);
}

aspect production stringConst
top::Term ::= s::String
{
  top.ocamlExpr = ocamlString(s);
}

aspect production appTerm
top::Term ::= constructor::QName args::TermList
{
  -- Old (module-qualified) version:
  -- top.ocamlExpr =
  --   ocamlConstructor(
  --     capitalizeFirst(if constructor.isQualified
  --                     then constructor.ocamlString
  --                     else constructor.fullConstrName.ocamlString),
  --     args.ocamlExprs);
  top.ocamlExpr =
    ocamlConstructor(
      capitalizeFirst(if constructor.isQualified
                      then constructor.base
                      else constructor.fullConstrName.base),
      args.ocamlExprs);
}

aspect production tupleTerm
top::Term ::= contents::TermList
{
  top.ocamlExpr = ocamlTuple(contents.ocamlExprs);
}

aspect production nilTerm
top::Term ::=
{
  top.ocamlExpr = ocamlList([]);
}

aspect production consTerm
top::Term ::= hd::Term tl::Term
{
  top.ocamlExpr =
    case tl.ocamlExpr of
    | ocamlList(exprs) -> ocamlList(hd.ocamlExpr :: exprs)
    | _ -> ocamlVar("(" ++ hd.ocamlExpr.pp ++ " :: " ++ tl.ocamlExpr.pp ++ ")")
    end;
}

aspect production ascriptionTerm
top::Term ::= tm::Term ty::Type
{
  top.ocamlExpr = tm.ocamlExpr;
}

aspect production nilTermList
top::TermList ::=
{
  top.ocamlExprs = [];
  top.decoratedTermList = [];
}

aspect production consTermList
top::TermList ::= t::Term rest::TermList
{
  top.ocamlExprs = t.ocamlExpr :: rest.ocamlExprs;
  top.decoratedTermList = t :: rest.decoratedTermList;
}
