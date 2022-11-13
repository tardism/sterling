grammar sos:translation:semantic:lambdaProlog;


attribute lp<LambdaPrologTerm> occurs on Term;

aspect production const
top::Term ::= name::QName
{
  top.lp =
      constLambdaPrologTerm(name.fullConstrName.lpConstructorName);
}


aspect production var
top::Term ::= name::String
{
  top.lp = varLambdaPrologTerm(name);
}


aspect production num
top::Term ::= int::Integer
{
  top.lp = integerLambdaPrologTerm(int);
}


aspect production stringConst
top::Term ::= s::String
{
  top.lp = stringLambdaPrologTerm(s);
}


aspect production appTerm
top::Term ::= constructor::QName args::TermList
{
  top.lp = foldl(applicationLambdaPrologTerm,
                 constLambdaPrologTerm(
                    constructor.fullConstrName.lpConstructorName),
                 args.lp);
}


aspect production ascriptionTerm
top::Term ::= tm::Term ty::Type
{
  top.lp = tm.lp;
}





attribute lp<[LambdaPrologTerm]>, pcVar occurs on TermList;

aspect production nilTermList
top::TermList ::=
{
  top.lp = [];

  top.pcVar = error("Should not access pcVar on nilTermList");
}


aspect production consTermList
top::TermList ::= t::Term rest::TermList
{
  top.lp = t.lp::rest.lp;

  top.pcVar =
      case top.expectedPC, t of
      | just(0), var(name) -> name
      | _, _ -> rest.pcVar
      end;
}

