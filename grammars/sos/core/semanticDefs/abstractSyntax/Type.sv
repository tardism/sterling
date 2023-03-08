grammar sos:core:semanticDefs:abstractSyntax;


--replace variables with new, rigid variables
synthesized attribute rigidizeSubst::Substitution;
synthesized attribute rigidize<a>::a;

synthesized attribute isPC::Boolean;
synthesized attribute foundPC::Boolean;


attribute
   rigidize<Type>, rigidizeSubst,
   isExtensible,
   isPC
occurs on Type;

aspect production nameType
top::Type ::= name::QName
{
  top.rigidize = top;
  top.rigidizeSubst = emptySubst();

  --assume all name types are extensible
  top.isExtensible = true;
  top.isPC = false;
}


aspect production varType
top::Type ::= name::String
{
  top.rigidize =
      rigidVarType(name, genInt(), location=top.location);
  top.rigidizeSubst = addSubst(name, top.rigidize, emptySubst());

  --assume variable types are not extensible, as undetermined
  top.isExtensible = false;
  top.isPC = false;
}


--used for checking rules for polymorphic judgments
abstract production rigidVarType
top::Type ::= name::String index::Integer
{
  top.pp = "<rigid ty var " ++ name ++ " " ++ toString(index) ++ ">";

  top.substituted = top;

  top.upSubst =
      case top.unifyWith of
      | rigidVarType(n, i) when n == name && i == index ->
        top.downSubst
      | varType(v) -> addSubst(v, top, top.downSubst)
      | _ ->
        addErrSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                    top.pp, top.unifyLoc, top.downSubst)
      end;

  top.freshen = error("Should not freshen with rigid vars");
  top.freshenSubst = top.freshenSubst_down;

  top.rigidize = top;
  top.rigidizeSubst = emptySubst();

  top.isExtensible = false;
  top.isPC = false;

  top.type = top;

  top.isError = false;

  top.containsVars = false;

  forwards to varType(name ++ toString(index), location=top.location);
}


aspect production intType
top::Type ::=
{
  top.rigidize = top;
  top.rigidizeSubst = emptySubst();

  top.isExtensible = false;
  top.isPC = false;
}


aspect production stringType
top::Type ::=
{
  top.rigidize = top;
  top.rigidizeSubst = emptySubst();

  top.isExtensible = false;
  top.isPC = false;
}


aspect production tupleType
top::Type ::= tys::TypeList
{
  top.rigidize = tupleType(tys.rigidize, location=top.location);
  top.rigidizeSubst = tys.rigidizeSubst;

  top.isExtensible = false;
  top.isPC = false;
}


aspect production listType
top::Type ::= ty::Type
{
  top.rigidize = listType(ty.rigidize, location=top.location);
  top.rigidizeSubst = ty.rigidizeSubst;

  top.isExtensible = false;
  top.isPC = false;
}


aspect production errorType
top::Type ::=
{
  top.rigidize = top;
  top.rigidizeSubst = emptySubst();

  top.isExtensible = false;
  top.isPC = false;
}


abstract production pcType
top::Type ::= t::Type
{
  top.errors <-
      if t.isExtensible
      then []
      else [errorMessage("Type " ++ t.pp ++ " declared as primary " ++
                         "component is not extensible",
                         location=top.location)];

  top.isPC = true;

  forwards to t;
}





attribute
   rigidize<TypeList>, rigidizeSubst,
   pcIndex, foundPC
occurs on TypeList;

aspect production nilTypeList
top::TypeList ::=
{
  top.rigidize = top;
  top.rigidizeSubst = emptySubst();

  top.pcIndex = error("pcIndex on nilTypeList");
  top.foundPC = false;
}


aspect production consTypeList
top::TypeList ::= t::Type rest::TypeList
{
  local rigidizeSubstituted::TypeList =
      performSubstitutionTypeList(rest, t.rigidizeSubst);
  top.rigidize =
      consTypeList(t.rigidize, rigidizeSubstituted.rigidize,
                   location=top.location);
  top.rigidizeSubst =
      joinSubst(t.rigidizeSubst, rigidizeSubstituted.rigidizeSubst);

  top.pcIndex =
      if t.isPC
      then 0
      else rest.pcIndex + 1;
  top.foundPC = t.isPC || rest.foundPC;
  top.errors <-
      if t.isPC
      then if rest.foundPC
           then 
             [errorMessage("Two primary components declared for " ++
                           "same relation", location=top.location)]
           else []
      else [];
}


function rigidizeType
Type ::= ty::Type
{
  return ty.rigidize;
}


function rigidizeTypeList
TypeList ::= ty::TypeList
{
  return ty.rigidize;
}
