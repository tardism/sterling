grammar sos:core:files:abstractSyntax;


nonterminal Term with
   pp,
   moduleName,
   tyEnv, constructorEnv,
   type, upSubst, downSubst, finalSubst,
   errors,
   location;
propagate errors on Term;

abstract production const
top::Term ::= name::QName
{
  top.pp = name.pp;

  name.constructorEnv = top.constructorEnv;

  top.errors <- name.constrErrors;
  top.type = if name.constrFound
             then freshenType(name.constrType)
             else errorType(location=top.location);

  top.upSubst = top.downSubst;
}


abstract production var
top::Term ::= name::String
{
  top.pp = name;

  top.type =
      varType("__var_" ++ toString(genInt()), location=top.location);

  top.upSubst = top.downSubst;
}


abstract production num
top::Term ::= int::Integer
{
  top.pp = toString(int);

  top.type = intType(location=top.location);

  top.upSubst = top.downSubst;
}


abstract production stringConst
top::Term ::= s::String
{
  top.pp = "\"" ++ s ++ "\"";

  top.type = stringType(location=top.location);

  top.upSubst = top.downSubst;
}


abstract production appTerm
top::Term ::= constructor::QName args::TermList
{
  top.pp = constructor.pp ++ "(" ++ args.pp_comma ++ ")";

  args.moduleName = top.moduleName;

  constructor.constructorEnv = top.constructorEnv;
  args.tyEnv = top.tyEnv;
  args.constructorEnv = top.constructorEnv;

  top.errors <- constructor.constrErrors;
  top.type = if constructor.constrFound
             then freshenType(constructor.constrType)
             else errorType(location=top.location);

  local unifyArgs::TypeUnify =
        typeListUnify(args.types, constructor.constrTypeArgs);
  unifyArgs.downSubst = args.upSubst;

  args.downSubst = top.downSubst;
  top.upSubst = if constructor.constrFound
                then unifyArgs.upSubst
                else args.upSubst;
  args.finalSubst = top.finalSubst;
}


abstract production ascriptionTerm
top::Term ::= tm::Term ty::Type
{
  top.pp = "(" ++ tm.pp ++ " : " ++ ty.pp ++ ")";

  tm.moduleName = top.moduleName;

  tm.constructorEnv = top.constructorEnv;
  tm.tyEnv = top.tyEnv;
  ty.tyEnv = top.tyEnv;

  local unify::TypeUnify = typeUnify(tm.type, ty);
  tm.downSubst = top.downSubst;
  unify.downSubst = tm.upSubst;
  top.upSubst = unify.upSubst;
  tm.finalSubst = top.finalSubst;

  top.type = ty;
}





nonterminal TermList with
   pp_comma, pp_space,
   moduleName,
   tyEnv, constructorEnv,
   types, upSubst, downSubst, finalSubst,
   toList<Term>, len,
   errors,
   location;
propagate errors on TermList;

abstract production nilTermList
top::TermList ::=
{
  top.pp_comma = "";
  top.pp_space = "";

  top.toList = [];
  top.len = 0;

  top.upSubst = top.downSubst;

  top.types = nilTypeList(location=top.location);
}


abstract production consTermList
top::TermList ::= t::Term rest::TermList
{
  top.pp_comma = if rest.pp_comma == ""
                 then t.pp else t.pp ++ ", " ++ rest.pp_comma;
  top.pp_space = if rest.pp_space == ""
                 then t.pp else t.pp ++ " " ++ rest.pp_space;

  t.moduleName = top.moduleName;
  rest.moduleName = top.moduleName;

  t.tyEnv = top.tyEnv;
  t.constructorEnv = top.constructorEnv;
  rest.tyEnv = top.tyEnv;
  rest.constructorEnv = top.constructorEnv;

  top.toList = t::rest.toList;
  top.len = 1 + rest.len;

  t.downSubst = top.downSubst;
  rest.downSubst = t.upSubst;
  top.upSubst = rest.upSubst;
  t.finalSubst = top.finalSubst;
  rest.finalSubst = top.finalSubst;

  top.types = consTypeList(t.type, rest.types, location=top.location);
}

