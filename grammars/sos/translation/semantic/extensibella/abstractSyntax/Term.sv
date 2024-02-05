grammar sos:translation:semantic:extensibella:abstractSyntax;


attribute
   eb<ExtensibellaTerm>, isVar, argVars
occurs on Term;

aspect production const
top::Term ::= name::QName
{
  top.eb =
      nameExtensibellaTerm(name.fullConstrName.ebConstructorName);

  top.isVar = false;
  top.argVars = [];
}


aspect production var
top::Term ::= name::String
{
  top.eb = varExtensibellaTerm(name);

  top.isVar = true;
  top.argVars = [name];
}


aspect production num
top::Term ::= int::Integer
{
  top.eb = extensibellaIntegerTerm(int);

  top.isVar = false;
  top.argVars = [];
}


aspect production stringConst
top::Term ::= s::String
{
  top.eb = extensibellaStringTerm(s);

  top.isVar = false;
  top.argVars = [];
}


aspect production appTerm
top::Term ::= constructor::QName args::TermList
{
  top.eb = applicationExtensibellaTerm(
              constructor.fullConstrName.ebConstructorName,
              args.eb);

  top.isVar = false;
  top.argVars = [];
}


aspect production tupleTerm
top::Term ::= contents::TermList
{
  top.eb = foldr1(\ a::ExtensibellaTerm b::ExtensibellaTerm ->
                    applicationExtensibellaTerm("$pair_c", [a, b]),
                  contents.eb);

  top.isVar = false;
  top.argVars = [];
}


aspect production nilTerm
top::Term ::=
{
  top.eb = nilExtensibellaTerm();

  top.isVar = false;
  top.argVars = [];
}


aspect production consTerm
top::Term ::= hd::Term tl::Term
{
  top.eb = consExtensibellaTerm(hd.eb, tl.eb);

  top.isVar = false;
  top.argVars = [];
}


aspect production ascriptionTerm
top::Term ::= tm::Term ty::Type
{
  top.eb = tm.eb;

  top.isVar = tm.isVar;
  top.argVars = tm.argVars;
}





attribute
   eb<[ExtensibellaTerm]>, pcVar, allArgsVars, argVars
occurs on TermList;

aspect production nilTermList
top::TermList ::=
{
  top.eb = [];

  top.pcVar = error("Should not translate in the presence of errors");

  top.allArgsVars = true;
  top.argVars = [];
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

  top.allArgsVars = t.isVar && rest.allArgsVars;
  top.argVars = t.argVars ++ rest.argVars;
}

