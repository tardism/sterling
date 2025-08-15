grammar sos:translation:semantic:ocaml;

attribute ocamlExpr, ocamlJudgmentType, ocamlMatchTerm, ocamlLetReserve occurs on Judgment;
attribute ocamlBinOp occurs on BinOp;
attribute ocamlTopBinOp occurs on TopBinOp;
attribute ocamlConjunction occurs on JudgmentList;

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

aspect production relation
top::Judgment ::= rel::QName args::TermList
{
  local pcIndex::Integer = getIndexOfPrimaryArg(^rel, top.judgmentEnv);
  local letBindingExprs::[String] = map((.pp), drop(pcIndex+1, args.ocamlExprs));
  local letBinding::String = 
    if length(letBindingExprs) == 0 then "()"
    else if length(letBindingExprs) == 1 then head(letBindingExprs)
    else "(" ++ implode(", ", letBindingExprs) ++ ")";

  top.ocamlJudgmentType = rel.ocamlString;
  -- get first pcIndex+1 args 
  top.ocamlLetReserve = rel.ocamlString ++ " " ++ implode(" ", map((.pp), take(pcIndex, args.ocamlExprs)));
  top.ocamlMatchTerm = head(drop(pcIndex, args.ocamlExprs));
  top.ocamlExpr = 
    -- ocamlApplication(ocamlVar(rel.ocamlString), args.ocamlExprs);
    if top.isConclusion then
    ocamlVar(letBinding)
    else
    ocamlLet(letBinding, 
      ocamlApplication(ocamlVar(rel.ocamlString), 
      take(pcIndex+1, args.ocamlExprs)), ocamlVar(""));
}

aspect production negationRelation
top::Judgment ::= rel::QName args::TermList
{
  top.ocamlExpr = 
    ocamlApplication(ocamlVar("not"), 
      [ocamlApplication(ocamlVar(rel.ocamlString), args.ocamlExprs)]);
}

aspect production projJudgment
top::Judgment ::= args::TermList ty::QName t::Term projection::Term
{
  top.ocamlExpr = 
    ocamlApplication(ocamlVar("project_" ++ ty.ocamlString), 
      args.ocamlExprs ++ [t.ocamlExpr, projection.ocamlExpr]);
}

aspect production binOpJudgment
top::Judgment ::= t1::Term op::BinOp t2::Term result::Term
{
  top.ocamlExpr = op.ocamlBinOp(t1.ocamlExpr, t2.ocamlExpr, result.ocamlExpr);
}

aspect production topBinOpJudgment
top::Judgment ::= t1::Term op::TopBinOp t2::Term
{
  top.ocamlExpr = op.ocamlTopBinOp(t1.ocamlExpr, t2.ocamlExpr);
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

aspect production neqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, "<>", t2);
}

aspect production lessOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, "<", t2);
}

aspect production greaterOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, ">", t2);
}

aspect production leqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, "<=", t2);
}

aspect production geqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, ">=", t2);
}

aspect production nilJudgmentList
top::JudgmentList ::=
{
  top.ocamlConjunction = \ conclusion::OCamlExpr -> conclusion;
}

aspect production consJudgmentList
top::JudgmentList ::= j::Judgment rest::JudgmentList
{
  top.ocamlConjunction = 
    \ conclusion::OCamlExpr ->
      ocamlInfixOp(j.ocamlExpr, "", rest.ocamlConjunction(conclusion));
}