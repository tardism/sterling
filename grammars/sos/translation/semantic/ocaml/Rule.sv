grammar sos:translation:semantic:ocaml;

attribute ocamlDecls occurs on Rule;
attribute decoratedJudgmentList occurs on JudgmentList;
type DecoratedJudgment = Decorated Judgment with {isConclusion, judgmentEnv, constructorEnv};
synthesized attribute decoratedJudgmentList::[DecoratedJudgment];

function returnIsMatchIndex
Integer ::= judgments::[DecoratedJudgment]
{
  return returnIsMatchIndexHelper(judgments, 0);
}

function returnIsMatchIndexHelper
Integer ::= judgments::[DecoratedJudgment] currentIndex::Integer
{
  return case judgments of
  | [] -> -1  -- Not found, return -1
  | j::rest ->
      if j.isMatch
      then currentIndex
      else returnIsMatchIndexHelper(rest, currentIndex + 1)
  end;
}

function returnLastIsMatchIndex
Integer ::= judgments::[DecoratedJudgment]
{
  local len::Integer = length(judgments);
  local reversedJudgments::[DecoratedJudgment] = reverse(judgments);
  local firstInReversed::Integer = returnIsMatchIndex(reversedJudgments);
  return if firstInReversed == -1 then -1 else len - 1 - firstInReversed;
}

function processJudgmentList
OCamlExpr ::= judgments::[DecoratedJudgment] conclusion::OCamlExpr
{
  local firstMatchIndex::Integer = returnIsMatchIndex(judgments);
  return case judgments of
  | [] -> ^conclusion  -- No judgments, return conclusion directly
  | _ -> 
      if firstMatchIndex == -1 
      then -- No match patterns, process normally
        foldl(\ acc::OCamlExpr j::DecoratedJudgment -> 
                ocamlSequence(j.ocamlExpr, acc), 
              ^conclusion, reverse(judgments))
      else -- Has match patterns
        processWithMatches(judgments, firstMatchIndex, ^conclusion)
  end;
}

function processWithMatches
OCamlExpr ::= judgments::[DecoratedJudgment] firstMatchIndex::Integer conclusion::OCamlExpr
{
  local lastMatchIndex::Integer = returnLastIsMatchIndex(judgments);
  local beforeMatch::[DecoratedJudgment] = take(firstMatchIndex, judgments);
  local matchJudgments::[DecoratedJudgment] = 
    take(lastMatchIndex - firstMatchIndex + 1, drop(firstMatchIndex, judgments));
  local afterMatch::[DecoratedJudgment] = drop(lastMatchIndex + 1, judgments);
  local matchExprs::[OCamlExpr] = map((.ocamlExpr), matchJudgments);
  local matchResults::[OCamlExpr] = map((.matchRes), matchJudgments);
  local afterExpr::OCamlExpr = processJudgmentList(afterMatch, ^conclusion);
  local matchExpr::OCamlExpr = 
    ocamlMatch(
      ocamlTuple(matchExprs),
      ocamlTuple(matchResults),
      ^afterExpr
    );
  local beforeExpr::OCamlExpr = 
    foldr(\ j::DecoratedJudgment acc::OCamlExpr -> 
            ocamlSequence(j.ocamlExpr, acc), 
          ^matchExpr, beforeMatch);
  return ^beforeExpr;
}

aspect production extRule
top::Rule ::= premises::JudgmentList name::String conclusion:: Judgment
{
  local functionName::String = name;
  -- premises.ocamlLetReserve = conclusion.ocamlLetReserve;
  local decoratedJudgments::[DecoratedJudgment] = premises.decoratedJudgmentList;
  local functionBody::OCamlExpr = 
          processJudgmentList(decoratedJudgments, conclusion.ocamlExpr);
  local isMatch::Boolean = 
    if returnIsMatchIndex(decoratedJudgments) == -1 then false else true;
  top.ocamlDecls = 
    [ocamlMatchBranch(isMatch, conclusion.ocamlJudgmentType, conclusion.ocamlLetReserve, conclusion.ocamlMatchTerm, ^functionBody)];
}

aspect production defaultRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.ocamlDecls = [];
}

aspect production fixedRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  local functionName::String = name;
  local functionBody::OCamlExpr = 
    premises.ocamlConjunction(conclusion.ocamlExpr);
  
  top.ocamlDecls = 
    [ocamlLetDeclaration(functionName, ["a"], ^functionBody)];
}

aspect production nilJudgmentList
top::JudgmentList ::=
{
  top.ocamlConjunction = \ conclusion::OCamlExpr -> conclusion;
  top.decoratedJudgmentList = [];
}

aspect production consJudgmentList
top::JudgmentList ::= j::Judgment rest::JudgmentList
{
  top.ocamlConjunction = 
    \ conclusion::OCamlExpr ->
      ocamlInfixOp(j.ocamlExpr, "", rest.ocamlConjunction(conclusion));
  top.decoratedJudgmentList = j::rest.decoratedJudgmentList;
}