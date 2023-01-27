grammar sos:translation:main:silver;

synthesized attribute silverFunDefs::[SilverFunDef];

attribute
   silverFunDefs
occurs on MainFile;

aspect production mainFile
top::MainFile ::= moduleName::QName contents::MainDecls
{
  top.silverFunDefs = contents.silverFunDefs;
}





attribute
   silverFunDefs
occurs on MainDecls;

aspect production emptyMainDecls
top::MainDecls ::=
{
  top.silverFunDefs = [];
}


aspect production branchMainDecls
top::MainDecls ::= d1::MainDecls d2::MainDecls
{
  top.silverFunDefs = d1.silverFunDefs ++ d2.silverFunDefs;
}


aspect production funMainDecl
top::MainDecls ::= f::FunDecl
{
  top.silverFunDefs = f.silverFunDefs;
}





attribute
   silverFunDefs
occurs on FunDecl;

aspect production funDecl
top::FunDecl ::= name::String params::Params retTy::Type body::Expr
{
  local bodyStr::String = error("funDecl.bodyStr");
  top.silverFunDefs =
      [silverFunDef(funName(name), params.silverParams,
                    retTy.silverType, bodyStr)];
}





attribute
  silverParams
occurs on Params;

--[(param name, type)]
synthesized attribute silverParams::[(String, String)];

aspect production branchParams
top::Params ::= p1::Params p2::Params
{
  top.silverParams = p1.silverParams ++ p2.silverParams;
}


aspect production emptyParams
top::Params ::=
{
  top.silverParams = [];
}


aspect production oneParams
top::Params ::= name::String ty::Type
{
  top.silverParams = [(name, ty.silverType)];
}
