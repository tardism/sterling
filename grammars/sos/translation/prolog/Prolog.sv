grammar sos:translation:prolog;


nonterminal PrologTerm with
   pp,
   replaceVar, replaceVal, replaced<PrologTerm>;

abstract production constPrologTerm
top::PrologTerm ::= name::String
{
  top.pp = name;

  top.replaced = top;
}


abstract production varPrologTerm
top::PrologTerm ::= name::String
{
  top.pp = name;

  top.replaced =
      if top.replaceVar == name
      then top.replaceVal
      else top;
}


abstract production integerPrologTerm
top::PrologTerm ::= i::Integer
{
  top.pp = toString(i);

  top.replaced = top;
}


abstract production stringPrologTerm
top::PrologTerm ::= text::String
{
  top.pp = "\"" ++ text ++ "\"";

  top.replaced = top;
}


abstract production applicationTerm
top::PrologTerm ::= name::String args::PrologTermList
{
  top.pp = name ++ "(" ++ args.pp ++ ")";

  args.replaceVar = top.replaceVar;
  args.replaceVal = top.replaceVal;
  top.replaced = applicationTerm(name, args.replaced);
}





nonterminal PrologTermList with
   pp,
   replaceVar, replaceVal, replaced<PrologTermList>;

abstract production nilPrologTermList
top::PrologTermList ::=
{
  top.pp = "";

  top.replaced = top;
}


abstract production consPrologTermList
top::PrologTermList ::= t::PrologTerm rest::PrologTermList
{
  top.pp = if rest.pp == "" then t.pp else t.pp ++ ", " ++ rest.pp;

  t.replaceVar = top.replaceVar;
  t.replaceVal = top.replaceVal;
  rest.replaceVar = top.replaceVar;
  rest.replaceVal = top.replaceVal;
  top.replaced = consPrologTermList(t.replaced, rest.replaced);
}





nonterminal PrologFormula with
   pp,
   replaceVar, replaceVal, replaced<PrologFormula>;

abstract production termPrologFormula
top::PrologFormula ::= t::PrologTerm
{
  top.pp = t.pp;

  t.replaceVar = top.replaceVar;
  t.replaceVal = top.replaceVal;
  top.replaced = termPrologFormula(t.replaced);
}


abstract production andPrologFormula
top::PrologFormula ::= f1::PrologFormula f2::PrologFormula
{
  top.pp = f1.pp ++ ", " ++ f2.pp;

  f1.replaceVar = top.replaceVar;
  f1.replaceVal = top.replaceVal;
  f2.replaceVar = top.replaceVar;
  f2.replaceVal = top.replaceVal;
  top.replaced = andPrologFormula(f1.replaced, f2.replaced);
}


abstract production binOpPrologFormula
top::PrologFormula ::= t1::PrologTerm op::PrologBinOp t2::PrologTerm
{
  top.pp = t1.pp ++ op.pp ++ t2.pp;

  t1.replaceVar = top.replaceVar;
  t1.replaceVal = top.replaceVal;
  t2.replaceVar = top.replaceVar;
  t2.replaceVal = top.replaceVal;
  top.replaced = binOpPrologFormula(t1.replaced, op, t2.replaced);
}


abstract production isPrologFormula
top::PrologFormula ::= t1::PrologTerm op::PrologIsBinOp t2::PrologTerm result::PrologTerm
{
  top.pp = t1.pp ++ op.pp ++ t2.pp ++ " is " ++ result.pp;

  t1.replaceVar = top.replaceVar;
  t1.replaceVal = top.replaceVal;
  t2.replaceVar = top.replaceVar;
  t2.replaceVal = top.replaceVal;
  result.replaceVar = top.replaceVar;
  result.replaceVal = top.replaceVal;
  top.replaced =
      isPrologFormula(t1.replaced, op, t2.replaced, result.replaced);
}





nonterminal PrologBinOp with pp;

abstract production greaterPrologBinOp
top::PrologBinOp ::= --   >
{
  top.pp = " > ";
}


abstract production lessPrologBinOp
top::PrologBinOp ::= --   <
{
  top.pp = " < ";
}


abstract production geqPrologBinOp
top::PrologBinOp ::= --   >=
{
  top.pp = " >= ";
}


abstract production leqPrologBinOp
top::PrologBinOp ::= --   =<
{
  top.pp = " =< ";
}


abstract production eqPrologBinOp
top::PrologBinOp ::= --   =:=
{
  top.pp = " =:= ";
}


abstract production neqPrologBinOp
top::PrologBinOp ::= --   =\=
{
  top.pp = " =\\= ";
}





nonterminal PrologIsBinOp with pp;

abstract production plusPrologIsBinOp
top::PrologIsBinOp ::= --  +
{
  top.pp = " + ";
}


abstract production minusPrologIsBinOp
top::PrologIsBinOp ::= --  -
{
  top.pp = " - ";
}


abstract production multPrologIsBinOp
top::PrologIsBinOp ::= --  *
{
  top.pp = " * ";
}


abstract production divPrologIsBinOp
top::PrologIsBinOp ::= --  /
{
  top.pp = " / ";
}


abstract production powerPrologIsBinOp
top::PrologIsBinOp ::= --  **
{
  top.pp = " ** ";
}


abstract production integerDivPrologIsBinOp
top::PrologIsBinOp ::= --  //
{
  top.pp = " // ";
}


abstract production modulusPrologIsBinOp
top::PrologIsBinOp ::= --  mod
{
  top.pp = " mod ";
}





--I know there facts and rules are different, but I need a name to
--encompass both of them, and "rule" seems to fit best.
nonterminal PrologRule with pp;

abstract production factPrologRule
top::PrologRule ::= f::PrologTerm
{
  top.pp = f.pp ++ ".";
}


abstract production rulePrologRule
top::PrologRule ::= hd::PrologTerm body::PrologFormula
{
  top.pp = hd.pp ++ " :- " ++ body.pp ++ ".";
}





nonterminal PrologProgram with pp;

abstract production emptyPrologProgram
top::PrologProgram ::=
{
  top.pp = "";
}


abstract production rulePrologProgram
top::PrologProgram ::= r::PrologRule
{
  top.pp = r.pp ++ "\n";
}


abstract production branchPrologProgram
top::PrologProgram ::= p1::PrologProgram p2::PrologProgram
{
  top.pp = p1.pp ++ p2.pp;
}



--Build a Prolog program from the gathered rules
function buildPrologProgram
PrologProgram ::= rules::[(QName, Maybe<PrologFormula>, PrologTerm)]
{
  --sort by name so each relation's rules are together
  local sorted::[(QName, Maybe<PrologFormula>, PrologTerm)] =
      sortBy(\ p1::(QName, Maybe<PrologFormula>, PrologTerm)
               p2::(QName, Maybe<PrologFormula>, PrologTerm) ->
               p1.1.pp < p2.1.pp,
             rules);
  --build the Prolog rules
  local builtRules::[PrologRule] =
      map(\ p::(QName, Maybe<PrologFormula>, PrologTerm) ->
            case p.2 of
            | nothing() -> factPrologRule(p.3)
            | just(premises) -> rulePrologRule(p.3, premises)
            end,
          sorted);
  return
      foldr(\ r::PrologRule rest::PrologProgram ->
              branchPrologProgram(rulePrologProgram(r), rest),
            emptyPrologProgram(), builtRules);
}

