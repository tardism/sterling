grammar sos:translation:main:silver;

imports sos:core:common:abstractSyntax;
imports sos:core:semanticDefs:abstractSyntax;
imports sos:core:main:abstractSyntax;

nonterminal SilverFunDef with pp;

abstract production silverFunDef
top::SilverFunDef ::= name::String params::[(String, String)]
                      retTy::String body::String
{
  top.pp =
      "function " ++ name ++ "\n" ++ retTy ++ " ::= " ++
      implode(" ", map(\ p::(String, String) ->
                         p.1 ++ "::" ++ p.2, params)) ++
      " ioin::IOToken\n{\n" ++ body ++ "\n}";
}





nonterminal StmtDef with pp;

abstract production stmtTypedDef
top::StmtDef ::= name::String type::String body::String
{
  top.pp = error("stmtTypedDef.pp");
}





--functions for building common pieces of Silver expressions
function buildIOVal
String ::= ioin::String valin::String
{
  return "ioval(" ++ ioin ++ ", " ++ valin ++ ")";
}

function buildPrecedingCtx
String ::= valin::String
{
  return valin ++ ".iovalue";
}

function addAssigns
String ::= assigns::[(String, String)] currentCtx::String
{
  return foldr(\ p::(String, String) rest::String ->
                 "(" ++ p.1 ++ ", " ++ p.2 ++ ")::" ++ rest,
               currentCtx, assigns);
}

function funName
String ::= name::String
{
  return "fun__" ++ name;
}
