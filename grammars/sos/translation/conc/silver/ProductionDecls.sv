grammar sos:translation:conc:silver;


attribute
   silverConc<[SilverConcDecl]>
occurs on ConcreteSyntaxDecl;

aspect production newConcreteNonterminal
top::ConcreteSyntaxDecl ::= name::String ty::Type d::ConcreteProdDecls
{
  top.silverConc = nonterminalSilverConcDecl(name)::d.silverConc;
}


aspect production addConcreteNonterminal
top::ConcreteSyntaxDecl ::= name::QName d::ConcreteProdDecls
{
  top.silverConc = d.silverConc;
}





attribute
   silverConc<[SilverConcDecl]>
occurs on ConcreteProdDecls;

aspect production branchConcreteProdDecls
top::ConcreteProdDecls ::= d1::ConcreteProdDecls d2::ConcreteProdDecls
{
  top.silverConc = d1.silverConc ++ d2.silverConc;
}


aspect production concreteProdDecl
top::ConcreteProdDecls ::= p::ProductionElement t::Term
{
  local prodName::String = "concreteProd" ++ toString(genInt());
  local topName::String = error("concreteProdDecl.topName");
  local topTy::String = error("concreteprodDecl.topTy");
  top.silverConc =
      [productionSilverConcDecl(prodName, topName, topTy,
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
  top.silverConc = oneSilverConcProdChildren(var, n.pp);
}


--Name should refer to either a nonterminal or a terminal
aspect production unnamedProductionElement
top::ProductionElement ::= n::QName
{
  local concName::String = "genName" ++ toString(genInt());
  top.silverConc = oneSilverConcProdChildren(concName, n.pp);
}


aspect production emptyProductionElement
top::ProductionElement ::=
{
  top.silverConc = emptySilverConcProdChildren();
}
