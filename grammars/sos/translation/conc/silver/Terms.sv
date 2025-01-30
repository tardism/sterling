grammar sos:translation:conc:silver;


attribute
   silverConc<SilverConcTerm>
occurs on Term;

aspect production nameTerm
top::Term ::= name::QName
{
  top.silverConc = constSilverConcTerm(name.fullConstrName.pp);
}


aspect production applicationTerm
top::Term ::= name::QName args::TermList
{
  local n::String = name.fullConstrName.pp;
  top.silverConc = applicationSilverConcTerm(n, args.silverConc);
}


aspect production stringTerm
top::Term ::= s::String
{
  top.silverConc = stringLitSilverConcTerm(s);
}


aspect production intTerm
top::Term ::= i::Integer
{
  top.silverConc = intLitSilverConcTerm(i);
}


aspect production toIntTerm
top::Term ::= t::Term
{
  top.silverConc = toIntSilverConcTerm(t.silverConc);
}


aspect production prodIndex
top::Term ::= var::String
{
  local q::QName = head(prodElem).1;
  q.concreteEnv = top.concreteEnv;
  top.silverConc =
      if q.isConcreteNt
      then astSilverConcTerm(var)
      else lexemeSilverConcTerm(var);
}


aspect production substringTerm
top::Term ::= t::Term i1::Maybe<Integer> i2::Maybe<Integer>
{
  local ind1::SilverConcTerm =
      case i1 of
      | nothing() -> intLitSilverConcTerm(0)
      | just(x) -> intLitSilverConcTerm(x)
      end;
  local ind2::SilverConcTerm =
      case i2 of
      | nothing() -> lengthSilverConcTerm(t.silverConc)
      | just(x) when x < 0 ->
        plusArithSilverConcTerm(lengthSilverConcTerm(t.silverConc),
                                intLitSilverConcTerm(x))
      | just(x) -> intLitSilverConcTerm(x)
      end;
  top.silverConc = substringSilverConcTerm(^ind1, ^ind2, t.silverConc);
}


aspect production nilTerm
top::Term ::=
{
  top.silverConc = nilSilverConcTerm();
}


aspect production consTerm
top::Term ::= hd::Term tl::Term
{
  top.silverConc = consSilverConcTerm(hd.silverConc, tl.silverConc);
}


aspect production tupleTerm
top::Term ::= contents::TermList
{
  top.silverConc = tupleSilverConcTerm(contents.silverConc);
}





attribute
  silverConc<[SilverConcTerm]>
occurs on TermList;

aspect production singleTermList
top::TermList ::= t::Term
{
  top.silverConc = [t.silverConc];
}


aspect production branchTermList
top::TermList ::= t1::TermList t2::TermList
{
  top.silverConc = t1.silverConc ++ t2.silverConc;
}
