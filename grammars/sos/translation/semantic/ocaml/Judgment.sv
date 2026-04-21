grammar sos:translation:semantic:ocaml;

attribute ocamlExpr, ocamlJudgmentType, ocamlMatchTerm, ocamlLetReserve, isMatch,
  matchRes occurs on Judgment;
attribute ocamlBinOp occurs on BinOp;
attribute ocamlTopBinOp occurs on TopBinOp;
attribute ocamlConjunction occurs on JudgmentList;

-- Dummy values for missing judgment productions
aspect production negationRelation
top::Judgment ::= rel::QName args::TermList
{
  top.matchRes = ocamlVar("dummy_match");
  top.ocamlLetReserve = "dummy_let";
  top.ocamlMatchTerm = ocamlVar("dummy_term");
  top.ocamlJudgmentType = "dummy_judgment";
    
}

aspect production projJudgment  
top::Judgment ::= args::TermList rel::QName projection::Term ty::Term
{
  top.matchRes = ocamlVar("dummy_match");
  top.ocamlLetReserve = "dummy_let";
  top.ocamlMatchTerm = ocamlVar("dummy_term");
  top.ocamlJudgmentType = "dummy_judgment";
}

aspect production binOpJudgment
top::Judgment ::= t1::Term op::BinOp t2::Term result::Term
{
  top.matchRes = ocamlVar("dummy_match");
  top.ocamlLetReserve = "dummy_let";
  top.ocamlMatchTerm = ocamlVar("dummy_term");
  top.ocamlJudgmentType = "dummy_judgment";
}

aspect production topBinOpJudgment
top::Judgment ::= t1::Term op::TopBinOp t2::Term
{
  top.matchRes = ocamlVar("dummy_match");
  top.ocamlLetReserve = "dummy_let";
  top.ocamlMatchTerm = ocamlVar("dummy_term");
  top.ocamlJudgmentType = "dummy_judgment";
}

-- so, for example eval_a E A1 V1 O1, 
-- we loop through the args to get the primary component
-- and get the index of A1. 
function getIndexOfPrimaryArg
Integer ::= rel::QName judgmentEnv::Env<JudgmentEnvItem>
{
  -- Look up the judgment declaration to get its type signature
  local judgmentDecl::[JudgmentEnvItem] = lookupEnv(^rel, judgmentEnv);
  return case judgmentDecl of
  | [jenv] -> 
      -- Get the pcIndex from the judgment's type signature
      case jenv of
      | extJudgmentEnvItem(_, _, pcIndex) -> pcIndex
      | fixedJudgmentEnvItem(_, _) -> 1  -- Fixed judgments don't have PC, default to 1
      | _ -> 1 -- Default fallback
      end
  | _ -> 1  -- Default to index 1 if judgment not found
  end;
}

function ocamlTermVarGen
String ::= term::Decorated Term with {constructorEnv}
{
  return if term.isVariable then
    lowercaseFirst(term.pp)
  else
    "var" ++ toString(genInt());
}

function ocamlTermVarGen2
String ::= term::Decorated Term with {constructorEnv}
{
  return if term.isVariable then
    " _ "
  else
    term.ocamlExpr.pp;
}

function bindingLetOrMatch
String ::= T::Decorated Term with {constructorEnv}
{
  return if T.isVariable then
    -- " _ "
    T.ocamlExpr.pp
  else
    T.ocamlExpr.pp;
}

function isVariableOrAppTerm
Boolean ::= t::Term
{
  return if t.isVariable then
    true
  else
    case t of
    | appTerm(_, _) -> true
    | _ -> false
    end;
}

function isAllVariables
Boolean ::= args::[Term]
{
  return foldr(\ t::Term acc::Boolean -> isVariableOrAppTerm(t) && acc,
true, args);
}

aspect production relation
top::Judgment ::= rel::QName args::TermList
{
  local pcIndex::Integer =
    case lookupEnv(^rel, top.judgmentEnv) of
    | [] -> 1  -- Default fallback when judgment not found in environment
    | _ -> getIndexOfPrimaryArg(^rel, top.judgmentEnv)
    end;
  local qualifiedRel::String = rel.fullJudgment.name.ocamlString;
  local letBindingExprs::[String] = map((.pp), drop(pcIndex+1, args.ocamlExprs));
  local letBinding::String =
    if length(letBindingExprs) == 0 then "()"
    else if length(letBindingExprs) == 1 then head(letBindingExprs)
    else "(" ++ implode(", ", letBindingExprs) ++ ")";

  local matchBindingExprs::String =
    ocamlApplication(ocamlVar(qualifiedRel), take(pcIndex+1, args.ocamlExprs)).pp;

  top.matchRes = ocamlVar("(" ++ implode(", ", map(bindingLetOrMatch, drop(pcIndex+1, args.decoratedTermList))) ++ ")");
  local inputsTermList::[String] =
    map(ocamlTermVarGen, take(pcIndex+1, args.decoratedTermList));
  top.ocamlJudgmentType = qualifiedRel;
  top.ocamlLetReserve = qualifiedRel ++ " "
    ++ implode(" ", inputsTermList)
    ++ " = \n match "
    ++ implode(" , ", inputsTermList)
    ++ " with \n";
  top.ocamlMatchTerm = ocamlVar("( " ++ implode(", ", map(ocamlTermVarGen2, (take(pcIndex+1, args.decoratedTermList)))) ++ " )");
  top.ocamlExpr =
    if top.isConclusion then
    ocamlVar(letBinding)
    else if isAllVariables(args.toList) then
    ocamlLet(letBinding,
      ocamlApplication(ocamlVar(qualifiedRel),
      take(pcIndex+1, args.ocamlExprs)), ocamlVar(""))
    else
    ocamlVar(matchBindingExprs);

  top.isMatch = if top.isConclusion then
    false
  else if isAllVariables(args.toList) then
    false
  else
    true;
}

aspect production negationRelation
top::Judgment ::= rel::QName args::TermList
{
  top.ocamlExpr = 
    ocamlApplication(ocamlVar("not"), 
      [ocamlApplication(ocamlVar(rel.ocamlString), args.ocamlExprs)]);
  top.isMatch = false;
}

aspect production projJudgment
top::Judgment ::= args::TermList ty::QName t::Term projection::Term
{
  top.ocamlExpr = 
    ocamlApplication(ocamlVar("project_" ++ ty.ocamlString), 
      args.ocamlExprs ++ [t.ocamlExpr, projection.ocamlExpr]);
  top.isMatch = false;
}

aspect production binOpJudgment
top::Judgment ::= t1::Term op::BinOp t2::Term result::Term
{
  top.ocamlExpr = op.ocamlBinOp(t1.ocamlExpr, t2.ocamlExpr, result.ocamlExpr);
  top.isMatch = false;
}

aspect production topBinOpJudgment
top::Judgment ::= t1::Term op::TopBinOp t2::Term
{
  top.ocamlExpr = op.ocamlTopBinOp(t1.ocamlExpr, t2.ocamlExpr);
  top.isMatch = false;
}

aspect production plusOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlLet(result.pp, 
        ocamlInfixOp(t1, "+", t2), 
        ocamlVar(""));  
}

aspect production minusOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlLet(result.pp, 
        ocamlInfixOp(t1, "-", t2), 
        ocamlVar(""));  
}

aspect production multOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlLet(result.pp, 
        ocamlInfixOp(t1, "*", t2), 
        ocamlVar(""));  
}

aspect production divOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlLet(result.pp, 
        ocamlInfixOp(t1, "/", t2), 
        ocamlVar(""));  
}

aspect production modOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlLet(result.pp, 
        ocamlInfixOp(t1, "mod", t2), 
        ocamlVar(""));  
}

aspect production appendOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlLet(result.pp, 
        ocamlInfixOp(t1, "@", t2), 
        ocamlVar(""));  
}

aspect production eqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, "=", t2);
}

aspect production ocamlEqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlIf(ocamlInfixOp(t1, "==", t2));
}

aspect production neqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlIf(ocamlInfixOp(t1, "<>", t2));
}

aspect production lessOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlIf(ocamlInfixOp(t1, "<", t2));
}

aspect production greaterOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
     ocamlIf( ocamlInfixOp(t1, ">", t2));
}

aspect production leqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlIf(ocamlInfixOp(t1, "<=", t2));
}

aspect production geqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlIf(ocamlInfixOp(t1, ">=", t2));
}

