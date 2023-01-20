grammar sos:translation:conc:silver;


attribute
   silverConc<[SilverConcDecl]>
occurs on ConcreteSyntaxDecl;

aspect production newConcreteNonterminal
top::ConcreteSyntaxDecl ::= name::String ty::Type d::ConcreteProdDecls
{
  local silverName::String = toQName(name, top.location).silverConcNt;
  top.silverConc =
      nonterminalSilverConcDecl(silverName)::d.silverConc;
  d.silverConcTopTy = fullName.silverConcNt;
}


aspect production addConcreteNonterminal
top::ConcreteSyntaxDecl ::= name::QName d::ConcreteProdDecls
{
  top.silverConc = d.silverConc;
  d.silverConcTopTy = name.fullConcreteName.silverConcNt;
}





attribute
   silverConc<[SilverConcDecl]>, silverConcTopTy
occurs on ConcreteProdDecls;

inherited attribute silverConcTopTy::String;

aspect production branchConcreteProdDecls
top::ConcreteProdDecls ::= d1::ConcreteProdDecls d2::ConcreteProdDecls
{
  top.silverConc = d1.silverConc ++ d2.silverConc;
  d1.silverConcTopTy = top.silverConcTopTy;
  d2.silverConcTopTy = top.silverConcTopTy;
}


aspect production concreteProdDecl
top::ConcreteProdDecls ::= p::ProductionElement t::Term
{
  local prodName::String = "concreteProd" ++ toString(genInt());
  local topName::String = "top" ++ toString(genInt());
  top.silverConc =
      [productionSilverConcDecl(prodName, topName, top.silverConcTopTy,
          p.silverConc, silverConcAstEq(topName, t.silverConc))];
}





attribute
   silverConc<SilverConcProdChildren>
occurs on ProductionElement;

aspect production branchProductionElement
top::ProductionElement ::= d1::ProductionElement d2::ProductionElement
{
  top.silverConc =
      branchSilverConcProdChildren(d1.silverConc, d2.silverConc);
}


--Name should refer to either a nonterminal or a terminal
aspect production nameProductionElement
top::ProductionElement ::= var::String n::QName
{
  top.silverConc = oneSilverConcProdChildren(var, n.silverConc);
}


--Name should refer to either a nonterminal or a terminal
aspect production unnamedProductionElement
top::ProductionElement ::= n::QName
{
  local concName::String = "genName" ++ toString(genInt());
  top.silverConc = oneSilverConcProdChildren(concName, n.silverConc);
}


aspect production emptyProductionElement
top::ProductionElement ::=
{
  top.silverConc = emptySilverConcProdChildren();
}
