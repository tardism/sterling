grammar sos:translation:semantic:ocaml;

nonterminal OCamlType with pp;

abstract production ocamlIntType
top::OCamlType ::=
{
  top.pp = "int";
}

abstract production ocamlStringType
top::OCamlType ::=
{
  top.pp = "string";
}

abstract production ocamlBoolType
top::OCamlType ::=
{
  top.pp = "bool";
}

abstract production ocamlListType
top::OCamlType ::= ty::OCamlType
{
  top.pp = ty.pp ++ " list";
}

abstract production ocamlTupleType
top::OCamlType ::= types::[OCamlType]
{
  top.pp = "(" ++ implode(" * ", map((.pp), types)) ++ ")";
}

abstract production ocamlVariantType
top::OCamlType ::= name::String
{
  top.pp = name;
}

abstract production ocamlFunctionType
top::OCamlType ::= argTy::OCamlType retTy::OCamlType
{
  top.pp = argTy.pp ++ " -> " ++ retTy.pp;
}

nonterminal OCamlPattern with pp;

abstract production ocamlVarPattern
top::OCamlPattern ::= name::String
{
  top.pp = name;
}

abstract production ocamlWildcardPattern
top::OCamlPattern ::=
{
  top.pp = "_";
}

abstract production ocamlConstPattern
top::OCamlPattern ::= value::String
{
  top.pp = value;
}

abstract production ocamlConstructorPattern
top::OCamlPattern ::= name::String args::[OCamlPattern]
{
  top.pp = if null(args)
           then name
           else name ++ " (" ++ implode(", ", map((.pp), args)) ++ ")";
}

abstract production ocamlTuplePattern
top::OCamlPattern ::= patterns::[OCamlPattern]
{
  top.pp = "(" ++ implode(", ", map((.pp), patterns)) ++ ")";
}

abstract production ocamlListPattern
top::OCamlPattern ::= patterns::[OCamlPattern]
{
  top.pp = "[" ++ implode("; ", map((.pp), patterns)) ++ "]";
}

nonterminal OCamlExpr with pp, openIfCount, openMatchCount;

abstract production ocamlVar
top::OCamlExpr ::= name::String
{
  top.pp = name;
  top.openIfCount = 0;
  top.openMatchCount = 0;
}

abstract production ocamlInt
top::OCamlExpr ::= value::Integer
{
  top.pp = toString(value);
  top.openIfCount = 0;
  top.openMatchCount = 0;
}

abstract production ocamlString
top::OCamlExpr ::= value::String
{
  top.pp = "\"" ++ value ++ "\"";
  top.openIfCount = 0;
  top.openMatchCount = 0;
}

abstract production ocamlBool
top::OCamlExpr ::= value::Boolean
{
  top.pp = if value then "true" else "false";
  top.openIfCount = 0;
  top.openMatchCount = 0;
}

abstract production ocamlSequence
top::OCamlExpr ::= first::OCamlExpr second::OCamlExpr
{
  top.pp = case first of
           | ocamlIf(_) -> first.pp ++ second.pp ++  "\n else "
           | _ -> first.pp ++ "\n" ++ second.pp ++ " "
           end;
  top.openIfCount = case first of
                    | ocamlIf(_) -> 1 + second.openIfCount
                    | _ -> second.openIfCount
                    end;
  top.openMatchCount = second.openMatchCount;
}

abstract production ocamlConstructor
top::OCamlExpr ::= name::String args::[OCamlExpr]
{
  top.pp = if null(args)
           then name
           else "(" ++ name ++ " (" ++ implode(", ", map((.pp), args)) ++ ")" ++ ")";
  top.openIfCount = 0;
  top.openMatchCount = 0;
}

abstract production ocamlTuple
top::OCamlExpr ::= exprs::[OCamlExpr]
{
  top.pp = "(" ++ implode(", ", map((.pp), exprs)) ++ ")";
  top.openIfCount = 0;
  top.openMatchCount = 0;
}

abstract production ocamlList
top::OCamlExpr ::= exprs::[OCamlExpr]
{
  top.pp = "[" ++ implode("; ", map((.pp), exprs)) ++ "]";
  top.openIfCount = 0;
  top.openMatchCount = 0;
}

abstract production ocamlApplication
top::OCamlExpr ::= func::OCamlExpr args::[OCamlExpr]
{
  top.pp = func.pp ++ " " ++ implode(" ", map((.pp), args));
  top.openIfCount = 0;
  top.openMatchCount = 0;
}

abstract production ocamlInfixOp
top::OCamlExpr ::= left::OCamlExpr op::String right::OCamlExpr
{
  top.pp = " " ++ left.pp ++ " " ++ op ++ " " ++ right.pp ++ " ";
  top.openIfCount = 0;
  top.openMatchCount = 0;
}
abstract production ocamlMatchEnd
top::OCamlExpr ::= left::OCamlExpr op::String right::OCamlExpr
{
  top.pp = " " ++ left.pp ++ " " ++ op ++ " " ++ right.pp ++ " ";
  top.openIfCount = 0;
  top.openMatchCount = 0;
}

abstract production ocamlLet
top::OCamlExpr ::= name::String value::OCamlExpr body::OCamlExpr
{
  top.pp = "let " ++ name ++ " = " ++ value.pp ++ " in\n" ++ body.pp;
  top.openIfCount = body.openIfCount;
  top.openMatchCount = body.openMatchCount;
}

abstract production ocamlMatch
top::OCamlExpr ::= expr::OCamlExpr cases::OCamlExpr afterExpr::OCamlExpr
{
  top.pp = "(match " ++ expr.pp ++ " with\n" ++
          cases.pp ++ " -> " ++ afterExpr.pp ++ "\n";
  top.openIfCount = afterExpr.openIfCount;
  top.openMatchCount = 1 + afterExpr.openMatchCount;
}

abstract production ocamlIf
top::OCamlExpr ::= cond::OCamlExpr
{
  top.pp = "(if " ++ cond.pp ++ " then \n   ";
  top.openIfCount = 1;
  top.openMatchCount = 0;
}

nonterminal OCamlCase with pp;

abstract production ocamlCase
top::OCamlCase ::= pattern::OCamlPattern guard::Maybe<OCamlExpr> expr::OCamlExpr
{
  local guardStr::String = 
    case guard of
    | just(g) -> " when " ++ g.pp
    | nothing() -> ""
    end;
  top.pp = "| " ++ pattern.pp ++ guardStr ++ " -> " ++ expr.pp;
}

nonterminal OCamlTypeDecl with pp;

abstract production ocamlVariantDecl
top::OCamlTypeDecl ::= name::String constructors::[OCamlConstructor]
{
  top.pp = "type " ++ name ++ " =\n" ++
           implode("\n", map((.pp), constructors));
}

nonterminal OCamlConstructor with pp;

abstract production ocamlVariantConstructor
top::OCamlConstructor ::= name::String types::[OCamlType]
{
  local typeStr::String = 
    if null(types)
    then ""
    else " of " ++ implode(" * ", map((.pp), types));
  top.pp = "  | " ++ name ++ typeStr;
}

nonterminal OCamlDecl with pp;
attribute ocamlRuleType, ocamlLetReserve, ocamlMatchTerm, isMatch, openIfCount, openMatchCount occurs on OCamlDecl;

abstract production ocamlLibraryDecl
top::OCamlDecl ::=
{
  -- top.pp = "let rec lookup e x = match e with \n | [] -> raise (Failure (\" not found \"))  \n | (y, v) :: rest -> if x = y then v else lookup rest x";
  top.pp = "";
  top.ocamlRuleType = "unknown";
  top.ocamlLetReserve = "";
  top.ocamlMatchTerm = ocamlVar("unknown");
  top.isMatch = false;
  top.openIfCount = 0;
  top.openMatchCount = 0;
}

abstract production ocamlTypeDeclaration
top::OCamlDecl ::= decl::OCamlTypeDecl
{
  top.pp = decl.pp;
  top.ocamlRuleType = "type";
  top.ocamlLetReserve = "";
  top.ocamlMatchTerm = ocamlVar("unknown");
  top.isMatch = false;
  top.openIfCount = 0;
  top.openMatchCount = 0;
}

abstract production ocamlLetDeclaration
top::OCamlDecl ::= name::String params::[String] body::OCamlExpr
{
  local paramStr::String =
    if null(params)
    then ""
    else " " ++ implode(" ", params);
  top.pp = "let " ++ name ++ paramStr ++ " =\n  " ++ body.pp;
  top.ocamlRuleType = "unknown";
  top.ocamlLetReserve = name ++ paramStr;
  top.ocamlMatchTerm = ocamlVar(name);
  top.isMatch = false;
  top.openIfCount = body.openIfCount;
  top.openMatchCount = body.openMatchCount;
}

abstract production ocamlMatchBranch
top::OCamlDecl ::= isMatch::Boolean ruleType::String ocamlLetReserve::String matchTerm::OCamlExpr body::OCamlExpr
{
  top.pp = "| " ++ matchTerm.pp ++ " ->\n" ++ body.pp;
  top.ocamlLetReserve = ocamlLetReserve;
  top.ocamlRuleType = ruleType;
  top.ocamlMatchTerm = ^matchTerm;
  top.isMatch = isMatch;
  top.openIfCount = body.openIfCount;
  top.openMatchCount = body.openMatchCount;
}

function arrowEndString
  String ::= d::OCamlDecl
  {
    local s::String = d.pp;
    local arrowPos::Integer = indexOf("->", s);
    local arrowIndex::Integer = if arrowPos >= 0
           then arrowPos + 1  -- position of '>'
           else 0;              -- not found
    return if arrowIndex > 0
      then substring(arrowIndex+1, length(s), s)

      else s;  -- return the original string if no arrow found
  }

function help2
  String ::= d::OCamlDecl
{
  return if d.isMatch
    then " | _ -> \n" ++ arrowEndString(^d)
  else arrowEndString(^d);
}
function closeOpenIfs
String ::= count::Integer
{
  return if count > 0
  then "raise (Failure \"should not reach here\")" ++ implode("", repeat(")", count))
  else "";
}

function closeOpenMatches
String ::= count::Integer
{
  return if count > 0
  then implode("", repeat("\n| _ -> raise (Failure \"should not reach here\"))", count))
  else "";
}

function closeOpenExprs
String ::= ifCount::Integer matchCount::Integer
{
  return closeOpenIfs(ifCount) ++ closeOpenMatches(matchCount);
}

function help1
String ::= decls::[OCamlDecl]
{
  return if length(decls) == 1
  then head(decls).pp ++ closeOpenExprs(head(decls).openIfCount, head(decls).openMatchCount)
  else if head(tail(decls)).isMatch
    then head(decls).pp ++ implode("", map(help2, tail(decls)))
      ++ " | _ -> raise (Failure \"should not reach here\")"
      ++ implode("", repeat(")", length(decls))) ++ "\n"
    else head(decls).pp ++ implode("", map(help2, tail(decls)))
      ++ " raise (Failure \"should not reach here\")"
      ++ implode("", repeat(")", length(decls))) ++ "\n";
}


--   local temp::[OCamlDecl] = map( 
    -- \decls::[OCamlDecl] -> ocamlLetFull(decls), groupedByRuleType);  
  
abstract production ocamlLetFull
top::OCamlDecl ::= bodies::[OCamlDecl]
{
  local groupByMatchTerm::[[OCamlDecl]] = 
    groupBy(\p1::OCamlDecl p2::OCamlDecl 
            -> p1.ocamlMatchTerm.pp == p2.ocamlMatchTerm.pp, bodies); 
    
  top.pp = "let rec " ++ head(bodies).ocamlLetReserve ++ 
    implode("\n", map(help1, groupByMatchTerm)) ++ "\n| _ -> raise (Failure \"Match the unexpected case, consider type error?\")";
  top.ocamlRuleType = 
    if null(bodies) 
    then "unknown"
    else head(bodies).ocamlRuleType;  -- Use rule type from first body
  top.isMatch = false;
  top.ocamlLetReserve = if null(bodies) then "" else head(bodies).ocamlLetReserve;
  top.ocamlMatchTerm = if null(bodies) then ocamlVar("unknown") else head(bodies).ocamlMatchTerm;
  top.openIfCount = 0;
  top.openMatchCount = 0;
}


nonterminal OCamlProgram with pp;

function isTypeDecl
Boolean ::= decl::OCamlDecl
{
  return case decl of
         | ocamlTypeDeclaration(_) -> true
         | _ -> false
         end;
}

function isLibraryDecl
Boolean ::= decl::OCamlDecl
{
  return case decl of
         | ocamlLibraryDecl() -> true
         | _ -> false
         end;
}

abstract production ocamlProgram
top::OCamlProgram ::= decls::[OCamlDecl]
{
  top.pp = implode("\n\n", map((.pp), decls)) ++ "\n";
}
