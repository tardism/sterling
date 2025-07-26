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

nonterminal OCamlExpr with pp;

abstract production ocamlVar
top::OCamlExpr ::= name::String
{
  top.pp = name;
}

abstract production ocamlInt
top::OCamlExpr ::= value::Integer
{
  top.pp = toString(value);
}

abstract production ocamlString
top::OCamlExpr ::= value::String
{
  top.pp = "\"" ++ value ++ "\"";
}

abstract production ocamlBool
top::OCamlExpr ::= value::Boolean
{
  top.pp = if value then "true" else "false";
}

abstract production ocamlConstructor
top::OCamlExpr ::= name::String args::[OCamlExpr]
{
  top.pp = if null(args)
           then name
           else name ++ " (" ++ implode(", ", map((.pp), args)) ++ ")";
}

abstract production ocamlTuple
top::OCamlExpr ::= exprs::[OCamlExpr]
{
  top.pp = "(" ++ implode(", ", map((.pp), exprs)) ++ ")";
}

abstract production ocamlList
top::OCamlExpr ::= exprs::[OCamlExpr]
{
  top.pp = "[" ++ implode("; ", map((.pp), exprs)) ++ "]";
}

abstract production ocamlApplication
top::OCamlExpr ::= func::OCamlExpr args::[OCamlExpr]
{
  top.pp = func.pp ++ " " ++ implode(" ", map((.pp), args));
}

abstract production ocamlInfixOp
top::OCamlExpr ::= left::OCamlExpr op::String right::OCamlExpr
{
  top.pp = "(" ++ left.pp ++ " " ++ op ++ " " ++ right.pp ++ ")";
}


abstract production ocamlLet
top::OCamlExpr ::= name::String value::OCamlExpr body::OCamlExpr
{
  top.pp = "let " ++ name ++ " = " ++ value.pp ++ " in\n" ++ body.pp;
}

abstract production ocamlMatch
top::OCamlExpr ::= expr::OCamlExpr cases::[OCamlCase]
{
  top.pp = "match " ++ expr.pp ++ " with\n" ++
           implode("\n", map((.pp), cases));
}

abstract production ocamlIf
top::OCamlExpr ::= cond::OCamlExpr thenExpr::OCamlExpr elseExpr::OCamlExpr
{
  top.pp = "if " ++ cond.pp ++ " then " ++ thenExpr.pp ++ " else " ++ elseExpr.pp;
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

abstract production ocamlTypeDeclaration
top::OCamlDecl ::= decl::OCamlTypeDecl
{
  top.pp = decl.pp;
}

abstract production ocamlLetDeclaration
top::OCamlDecl ::= name::String params::[String] body::OCamlExpr
{
  local paramStr::String = 
    if null(params)
    then ""
    else " " ++ implode(" ", params);
  top.pp = "let " ++ name ++ paramStr ++ " =\n  " ++ body.pp;
}

abstract production ocamlRecursiveDeclaration
top::OCamlDecl ::= name::String params::[String] body::OCamlExpr
{
  local paramStr::String = 
    if null(params)
    then ""
    else " " ++ implode(" ", params);
  top.pp = "let rec " ++ name ++ paramStr ++ " =\n  " ++ body.pp;
}

nonterminal OCamlProgram with pp;

abstract production ocamlProgram
top::OCamlProgram ::= decls::[OCamlDecl]
{
  top.pp = implode("\n\n", map((.pp), decls)) ++ "\n";
}
