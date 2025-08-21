grammar sos:translation:semantic:ocaml;

attribute ocamlDecls occurs on Rule;

function returnIsMatchIndex
Integer ::= judgments::[Judgment]
{
  return returnIsMatchIndexHelper(judgments, 0);
}

function returnIsMatchIndexHelper
Integer ::= judgments::[Judgment] currentIndex::Integer
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
Integer ::= judgments::[Judgment]
{
  local len::Integer = length(judgments);
  local reversedJudgments::[Judgment] = reverse(judgments);
  local firstInReversed::Integer = returnIsMatchIndex(reversedJudgments);
  return if firstInReversed == -1 then -1 else len - 1 - firstInReversed;
}

function processJudgmentList
OCamlExpr ::= judgments::[Judgment] conclusion:: OCamlExpr
{
  local firstMatchIndex::Integer = returnIsMatchIndex(judgments);
  return case judgments of
  | [] -> ^conclusion  -- No judgments, return conclusion directly
  | _ -> 
      if firstMatchIndex == -1 
      then -- No match patterns, process normally
        foldl(\ acc::OCamlExpr j::Judgment -> 
                ocamlSequence(j.ocamlExpr, acc), 
              ^conclusion, reverse(judgments))
      else -- Has match patterns
        processWithMatches(judgments, firstMatchIndex, ^conclusion)
  end;
  -- return ^conclusion;
}

function processWithMatches
OCamlExpr ::= judgments::[Judgment] firstMatchIndex::Integer conclusion:: OCamlExpr
{
  local lastMatchIndex::Integer = returnLastIsMatchIndex(judgments);
  local beforeMatch::[Judgment] = take(firstMatchIndex, judgments);
  local matchJudgments::[Judgment] = 
    take(lastMatchIndex - firstMatchIndex + 1, drop(firstMatchIndex, judgments));
  local afterMatch::[Judgment] = drop(lastMatchIndex + 1, judgments);
  local beforeExpr::OCamlExpr = 
    foldr(\ j::Judgment acc::OCamlExpr -> 
            ocamlSequence(j.ocamlExpr, acc), 
          ^conclusion, beforeMatch);
  local matchExprs::[OCamlExpr] = map((.ocamlExpr), matchJudgments);
  local matchResults::[OCamlExpr] = map((.matchRes), matchJudgments);
  local afterExpr::OCamlExpr = processJudgmentList(afterMatch, ^conclusion);
  return ocamlSequence(
    ^beforeExpr,
    ocamlSequence(
      ocamlMatch(
        ocamlTuple(matchExprs),
        ocamlTuple(matchResults)
      ),
      ^afterExpr
    )
  );
}

aspect production extRule
top::Rule ::= premises::JudgmentList name::String conclusion:: Judgment
{
  local functionName::String = name;
  -- premises.ocamlLetReserve = conclusion.ocamlLetReserve;
  local functionBody::OCamlExpr = 
          processJudgmentList(premises.toList, conclusion.ocamlExpr);
  local t1::Integer = returnIsMatchIndex(premises.toList);
  local t2::Integer = returnLastIsMatchIndex(premises.toList);
  local t3 :: Integer = t1 + t2;
  top.ocamlDecls = 
    [ocamlMatchBranch(conclusion.isMatch, conclusion.ocamlJudgmentType, conclusion.ocamlLetReserve, conclusion.ocamlMatchTerm, ^functionBody)];
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
}

aspect production consJudgmentList
top::JudgmentList ::= j::Judgment rest::JudgmentList
{
  top.ocamlConjunction = 
    \ conclusion::OCamlExpr ->
      ocamlInfixOp(j.ocamlExpr, "", rest.ocamlConjunction(conclusion));
}