grammar sos:translation:semantic:lambdaProlog;


nonterminal LambdaPrologDeclaration with
   pp;

abstract production kindDeclaration
top::LambdaPrologDeclaration ::= kindName::String
{
  top.pp = "kind   " ++ kindName ++ "   type.\n";
}


abstract production typeDeclaration
top::LambdaPrologDeclaration ::= typeName::String ty::LambdaPrologType
{
  top.pp = "type   " ++ typeName ++ "   " ++ ty.pp ++ ".\n";
}





nonterminal LambdaPrologType with
   pp, isAtomic, isApp;

--for fewer parentheses in pp
synthesized attribute isAtomic::Boolean;
synthesized attribute isApp::Boolean;

abstract production oLPType
top::LambdaPrologType ::=
{
  top.pp = "o";
  top.isAtomic = true;
  top.isApp = false;
}


abstract production intLPType
top::LambdaPrologType ::=
{
  top.pp = "int";
  top.isAtomic = true;
  top.isApp = false;
}


abstract production stringLPType
top::LambdaPrologType ::=
{
  top.pp = "string";
  top.isAtomic = true;
  top.isApp = false;
}


abstract production nameLPType
top::LambdaPrologType ::= name::String
{
  top.pp = name;
  top.isAtomic = true;
  top.isApp = false;
}


abstract production varLPType
top::LambdaPrologType ::= varName::String
{
  top.pp = varName;
  top.isAtomic = true;
  top.isApp = false;
}


abstract production appLPType
top::LambdaPrologType ::= funTy::LambdaPrologType argTy::LambdaPrologType
{
  local funstr::String = if funTy.isAtomic || funTy.isApp
                         then funTy.pp
                         else "(" ++ funTy.pp ++ ")";
  local argStr::String = if argTy.isAtomic
                         then argTy.pp
                         else "(" ++ argTy.pp ++ ")";
  top.pp = funstr ++ " " ++ argStr;
  top.isAtomic = false;
  top.isApp = true;
}


abstract production arrowLPType
top::LambdaPrologType ::= ty1::LambdaPrologType ty2::LambdaPrologType
{
  top.pp =
    ( if ty1.isAtomic
      then ty1.pp
      else "(" ++ ty1.pp ++ ")" ) ++ " -> " ++ ty2.pp;
  top.isAtomic = false;
  top.isApp = false;
}





nonterminal LambdaPrologTerm with
   pp, isAtomic, isApp,
   replaceVar, replaceVal, replaced<LambdaPrologTerm>,
   vars;

abstract production constLambdaPrologTerm
top::LambdaPrologTerm ::= name::String
{
  top.pp = name;
  top.isAtomic = true;
  top.isApp = false;

  top.replaced = top;

  top.vars = [];
}


abstract production applicationLambdaPrologTerm
top::LambdaPrologTerm ::= t1::LambdaPrologTerm t2::LambdaPrologTerm
{
  local t1Str::String = if t1.isAtomic || t1.isApp
                        then t1.pp
                        else "(" ++ t1.pp ++ ")";
  local t2Str::String = if t2.isAtomic then t2.pp
                                       else "(" ++ t2.pp ++ ")";
  top.pp = t1Str ++ " " ++ t2Str;
  top.isAtomic = false;
  top.isApp = true;

  t1.replaceVar = top.replaceVar;
  t1.replaceVal = top.replaceVal;
  t2.replaceVar = top.replaceVar;
  t2.replaceVal = top.replaceVal;
  top.replaced = applicationLambdaPrologTerm(t1.replaced, t2.replaced);

  top.vars = union(t1.vars, t2.vars);
}


abstract production varLambdaPrologTerm
top::LambdaPrologTerm ::= name::String
{
  top.pp = name;
  top.isAtomic = true;
  top.isApp = false;

  top.replaced =
      if top.replaceVar == name
      then top.replaceVal
      else top;

  top.vars = [name];
}


abstract production integerLambdaPrologTerm
top::LambdaPrologTerm ::= i::Integer
{
  top.pp = toString(i);
  top.isAtomic = true;
  top.isApp = false;

  top.replaced = top;

  top.vars = [];
}


abstract production stringLambdaPrologTerm
top::LambdaPrologTerm ::= text::String
{
  top.pp = "\"" ++ text ++ "\"";
  top.isAtomic = true;
  top.isApp = false;

  top.replaced = top;

  top.vars = [];
}


abstract production abstractionLambdaPrologTerm
top::LambdaPrologTerm ::= var::String body::LambdaPrologTerm
{
  top.pp = var ++ "\\ " ++ body.pp;
  top.isAtomic = false;
  top.isApp = false;

  body.replaceVar = top.replaceVar;
  body.replaceVal = top.replaceVal;
  top.replaced =
      if var == top.replaceVar
      then top
      else abstractionLambdaPrologTerm(var, body.replaced);

  top.vars = remove(var, body.vars);
}





nonterminal LambdaPrologFormula with
   pp, isAtomic,
   replaceVar, replaceVal, replaced<LambdaPrologFormula>,
   vars;

abstract production termLambdaPrologFormula
top::LambdaPrologFormula ::= t::LambdaPrologTerm
{
  top.pp = t.pp;
  top.isAtomic = false;

  t.replaceVar = top.replaceVar;
  t.replaceVal = top.replaceVal;
  top.replaced = termLambdaPrologFormula(t.replaced);

  top.vars = t.vars;
}


abstract production notLambdaPrologFormula
top::LambdaPrologFormula ::= f::LambdaPrologFormula
{
  top.pp = "not " ++ if f.isAtomic then f.pp else "(" ++ f.pp ++ ")";
  top.isAtomic = false;

  f.replaceVar = top.replaceVar;
  f.replaceVal = top.replaceVal;
  top.replaced = notLambdaPrologFormula(f.replaced);

  top.vars = f.vars;
}


abstract production failLambdaPrologFormula
top::LambdaPrologFormula ::=
{
  top.pp = "fail";
  top.isAtomic = true;

  top.replaced = top;

  top.vars = [];
}


abstract production eqLambdaPrologFormula
top::LambdaPrologFormula ::= t1::LambdaPrologTerm t2::LambdaPrologTerm
{
  local t1Str::String = if t1.isAtomic then t1.pp
                                       else "(" ++ t1.pp ++ ")";
  local t2Str::String = if t2.isAtomic then t2.pp
                                       else "(" ++ t2.pp ++ ")";
  top.pp = t1Str ++ " = " ++ t2Str;
  top.isAtomic = false;

  t1.replaceVar = top.replaceVar;
  t1.replaceVal = top.replaceVal;
  t2.replaceVar = top.replaceVar;
  t2.replaceVal = top.replaceVal;
  top.replaced = eqLambdaPrologFormula(t1.replaced, t2.replaced);

  top.vars = union(t1.vars, t2.vars);
}


abstract production implicationLambdaPrologFormula
top::LambdaPrologFormula ::= t1::LambdaPrologFormula t2::LambdaPrologFormula
{
  local t1Str::String = if t1.isAtomic then t1.pp
                                       else "(" ++ t1.pp ++ ")";
  local t2Str::String = if t2.isAtomic then t2.pp
                                       else "(" ++ t2.pp ++ ")";
  top.pp = t1Str ++ " => " ++ t2Str;
  top.isAtomic = false;

  t1.replaceVar = top.replaceVar;
  t1.replaceVal = top.replaceVal;
  t2.replaceVar = top.replaceVar;
  t2.replaceVal = top.replaceVal;
  top.replaced = implicationLambdaPrologFormula(t1.replaced, t2.replaced);

  top.vars = union(t1.vars, t2.vars);
}


abstract production andLambdaPrologFormula
top::LambdaPrologFormula ::= f1::LambdaPrologFormula f2::LambdaPrologFormula
{
  top.pp = f1.pp ++ ", " ++ f2.pp;
  top.isAtomic = false;

  f1.replaceVar = top.replaceVar;
  f1.replaceVal = top.replaceVal;
  f2.replaceVar = top.replaceVar;
  f2.replaceVal = top.replaceVal;
  top.replaced = andLambdaPrologFormula(f1.replaced, f2.replaced);

  top.vars = union(f1.vars, f2.vars);
}


abstract production piLambdaPrologFormula
top::LambdaPrologFormula ::= bound::LambdaPrologFormula
{
  top.pp = "pi " ++ bound.pp;
  top.isAtomic = false;

  bound.replaceVar = top.replaceVar;
  bound.replaceVal = top.replaceVal;
  top.replaced = piLambdaPrologFormula(bound.replaced);

  top.vars = bound.vars;
}


abstract production sigmaLambdaPrologFormula
top::LambdaPrologFormula ::= bound::LambdaPrologFormula
{
  top.pp = "sigma " ++ bound.pp;
  top.isAtomic = false;

  bound.replaceVar = top.replaceVar;
  bound.replaceVal = top.replaceVal;
  top.replaced = sigmaLambdaPrologFormula(bound.replaced);

  top.vars = bound.vars;
}


abstract production binOpLambdaPrologFormula
top::LambdaPrologFormula ::= t1::LambdaPrologTerm op::LambdaPrologBinOp
                             t2::LambdaPrologTerm
{
  top.pp = t1.pp ++ op.pp ++ t2.pp;
  top.isAtomic = false;

  t1.replaceVar = top.replaceVar;
  t1.replaceVal = top.replaceVal;
  t2.replaceVar = top.replaceVar;
  t2.replaceVal = top.replaceVal;
  top.replaced = binOpLambdaPrologFormula(t1.replaced, op, t2.replaced);

  top.vars = union(t1.vars, t2.vars);
}


abstract production isLambdaPrologFormula
top::LambdaPrologFormula ::= t1::LambdaPrologTerm op::LambdaPrologIsBinOp
                             t2::LambdaPrologTerm result::LambdaPrologTerm
{
  top.pp = result.pp ++ " is " ++ t1.pp ++ op.pp ++ t2.pp;
  top.isAtomic = false;

  t1.replaceVar = top.replaceVar;
  t1.replaceVal = top.replaceVal;
  t2.replaceVar = top.replaceVar;
  t2.replaceVal = top.replaceVal;
  result.replaceVar = top.replaceVar;
  result.replaceVal = top.replaceVal;
  top.replaced =
      isLambdaPrologFormula(t1.replaced, op, t2.replaced, result.replaced);

  top.vars = union(t1.vars, union(t2.vars, result.vars));
}





nonterminal LambdaPrologBinOp with pp;

abstract production greaterLambdaPrologBinOp
top::LambdaPrologBinOp ::= --   >
{
  top.pp = " > ";
}


abstract production lessLambdaPrologBinOp
top::LambdaPrologBinOp ::= --   <
{
  top.pp = " < ";
}


abstract production geqLambdaPrologBinOp
top::LambdaPrologBinOp ::= --   >=
{
  top.pp = " >= ";
}


abstract production leqLambdaPrologBinOp
top::LambdaPrologBinOp ::= --   =<
{
  top.pp = " =< ";
}


abstract production eqLambdaPrologBinOp
top::LambdaPrologBinOp ::= --   =:=
{
  top.pp = " = ";
}





nonterminal LambdaPrologIsBinOp with pp;

abstract production plusLambdaPrologIsBinOp
top::LambdaPrologIsBinOp ::= --  +
{
  top.pp = " + ";
}


abstract production minusLambdaPrologIsBinOp
top::LambdaPrologIsBinOp ::= --  -
{
  top.pp = " - ";
}


abstract production multLambdaPrologIsBinOp
top::LambdaPrologIsBinOp ::= --  *
{
  top.pp = " * ";
}


abstract production integerDivLambdaPrologIsBinOp
top::LambdaPrologIsBinOp ::= -- div
{
  top.pp = " div ";
}


abstract production divLambdaPrologIsBinOp
top::LambdaPrologIsBinOp ::= --  /
{
  top.pp = " / ";
}


abstract production modulusLambdaPrologIsBinOp
top::LambdaPrologIsBinOp ::= --  mod
{
  top.pp = " mod ";
}


abstract production appendStringLambdaPrologIsBinOp
top::LambdaPrologIsBinOp ::= -- ^
{
  top.pp = " ^ ";
}





--I know there facts and rules are different, but I need a name to
--encompass both of them, and "rule" seems to fit best.
nonterminal LambdaPrologRule with
   pp,
   replaceVar, replaceVal, replaced<LambdaPrologRule>,
   vars;

abstract production factLambdaPrologRule
top::LambdaPrologRule ::= f::LambdaPrologTerm
{
  top.pp = f.pp ++ ".";

  f.replaceVar = top.replaceVar;
  f.replaceVal = top.replaceVal;
  top.replaced = factLambdaPrologRule(f.replaced);

  top.vars = f.vars;
}


abstract production ruleLambdaPrologRule
top::LambdaPrologRule ::= hd::LambdaPrologTerm body::LambdaPrologFormula
{
  top.pp = hd.pp ++ "  :-  " ++ body.pp ++ ".";

  hd.replaceVar = top.replaceVar;
  hd.replaceVal = top.replaceVal;
  body.replaceVar = top.replaceVar;
  body.replaceVal = top.replaceVal;
  top.replaced = ruleLambdaPrologRule(hd.replaced, body.replaced);

  top.vars = union(hd.vars, body.vars);
}





function buildSig
String ::= lpModuleName::String importedSOSModules::[String]
           decls::[LambdaPrologDeclaration]
{
  local importString::String =
        implode("\n",
                map(\ x::String ->
                      "accum_sig " ++ makeLPModuleName(x) ++ ".",
                    importedSOSModules));
  --kinds need to come first because they are referenced by types
  local sortedDecls::[LambdaPrologDeclaration] =
        sortBy(\ d1::LambdaPrologDeclaration
                 d2::LambdaPrologDeclaration ->
                 case d1, d2 of
                 | kindDeclaration(_), _ -> true
                 | _, kindDeclaration(_) -> false
                 | typeDeclaration(a, _), typeDeclaration(b, _) ->
                   a < b
                 end, decls);

  return "sig " ++ lpModuleName ++ ".\n\n" ++
         importString ++ "\n\n" ++
         --declare the pair we will use for tuples
         "kind pair*   type -> type -> type.\n" ++
         "type pair*   A -> B -> pair* A B.\n\n" ++
         implode("", map((.pp), sortedDecls)) ++ "\n";
}

function buildMod
String ::= lpModuleName::String importedSOSModules::[String]
           rules::[LambdaPrologRule]
{
  local importString::String =
        implode("\n",
                map(\ x::String ->
                      "accumulate " ++ makeLPModuleName(x) ++ ".",
                    importedSOSModules));
  return "module " ++ lpModuleName ++ ".\n\n" ++
         importString ++ "\n\n" ++
         implode("\n", map((.pp), rules)) ++ "\n";
}





function makeLPModuleName
String ::= sosExtModule::String
{
  return substitute(":", "-", sosExtModule);
}





function capitalize
String ::= name::String
{
  local first::String =
        case substring(0, 1, name) of
        | "a" -> "A" | "b" -> "B" | "c" -> "C" | "d" -> "D" | "e" -> "E"
        | "f" -> "F" | "g" -> "G" | "h" -> "H" | "i" -> "I" | "j" -> "J"
        | "k" -> "K" | "l" -> "L" | "m" -> "M" | "n" -> "N" | "o" -> "O"
        | "p" -> "P" | "q" -> "Q" | "r" -> "R" | "s" -> "S" | "t" -> "T"
        | "u" -> "U" | "v" -> "V" | "w" -> "W" | "x" -> "X" | "y" -> "Y"
        | "z" -> "Z" | x -> x
        end;
  return first ++ substring(1, length(name), name);
}

function freshName
String ::= base::String used::[String]
{
  return if contains(base, used)
         then freshName_help(base, 1, used)
         else base;
}
function freshName_help
String ::= base::String index::Integer used::[String]
{
  return if contains(base ++ toString(index), used)
         then freshName_help(base, index + 1, used)
         else base ++ toString(index);
}
