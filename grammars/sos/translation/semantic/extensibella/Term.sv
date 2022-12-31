grammar sos:translation:semantic:extensibella;


attribute
   eb<ExtensibellaTerm>
occurs on Term;

aspect production const
top::Term ::= name::QName
{
  top.eb =
      nameExtensibellaTerm(name.fullConstrName.ebConstructorName);
}


aspect production var
top::Term ::= name::String
{
  top.eb = varExtensibellaTerm(name);
}


aspect production num
top::Term ::= int::Integer
{
  top.eb = extensibellaIntegerTerm(int);
}


aspect production stringConst
top::Term ::= s::String
{
  top.eb = extensibellaStringTerm(s);
}


aspect production appTerm
top::Term ::= constructor::QName args::TermList
{
  top.eb = applicationExtensibellaTerm(
              constructor.fullConstrName.ebConstructorName,
              args.eb);
}


aspect production ascriptionTerm
top::Term ::= tm::Term ty::Type
{
  top.eb = tm.eb;
}





attribute
   eb<[ExtensibellaTerm]>, pcVar
occurs on TermList;

aspect production nilTermList
top::TermList ::=
{
  top.eb = [];

  top.pcVar = error("Should not translate in the presence of errors");
}


aspect production consTermList
top::TermList ::= t::Term rest::TermList
{
  top.eb = t.eb::rest.eb;

  top.pcVar =
      case top.expectedPC, t of
      | just(0), var(x) -> x
      | just(n), _ when n > 0 -> rest.pcVar
      | _, _ ->
        error("Should not translate in the presence of errors")
      end;
}

