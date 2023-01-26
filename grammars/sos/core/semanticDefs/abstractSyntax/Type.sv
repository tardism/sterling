grammar sos:core:semanticDefs:abstractSyntax;


inherited attribute subst::Substitution;
synthesized attribute substituted<a>::a;

inherited attribute unifyLoc::Location;
inherited attribute unifyWith<a>::a;

--freshen a type by creating new ty vars for each variable
synthesized attribute freshenSubst::Substitution;
synthesized attribute freshen<a>::a;
inherited attribute freshenLoc::Location;

--whether there are var types contained within
synthesized attribute containsVars::Boolean;

synthesized attribute isPC::Boolean;
synthesized attribute foundPC::Boolean;


attribute
   subst, substituted<Type>,
   unifyWith<Type>, unifyLoc, downSubst, upSubst,
   freshen<Type>, freshenSubst,
   isExtensible,
   isPC,
   containsVars
occurs on Type;

aspect production nameType
top::Type ::= name::QName
{
  top.substituted = nameType(name, location=top.location);

  top.upSubst =
      case top.unifyWith of
      | nameType(n) when n == name -> top.downSubst
      | nameType(n) ->
        addErrSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                    top.pp, top.unifyLoc, top.downSubst)
      | varType(v) -> addSubst(v, top, top.downSubst)
      | _ ->
        addErrSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                    top.pp, top.unifyLoc, top.downSubst)
      end;

  top.freshen = nameType(name, location=top.location);
  top.freshenSubst = emptySubst();

  --assume all name types are extensible
  top.isExtensible = true;
  top.isPC = false;

  top.containsVars = false;
}


abstract production varType
top::Type ::= name::String
{
  top.pp = name;

  top.substituted =
      case lookupSubst(name, top.subst) of
      | nothing() -> varType(name, location=top.location)
      | just(ty) -> ty
      end;

  top.upSubst =
      case top.unifyWith of
      | varType(v) when v == name -> top.downSubst
      | ty -> addSubst(name, ty, top.downSubst)
      end;

  top.freshen =
      varType("_var_" ++ name ++ "_" ++ toString(genInt()),
              location=top.location);
  top.freshenSubst = addSubst(name, top.freshen, emptySubst());

  --assume variable types are not extensible, as undetermined
  top.isExtensible = false;
  top.isPC = false;

  top.type = top;

  top.isError = false;

  top.containsVars = true;

  --This doesn't really fit for forwarding to anything actual
  forwards to errorType(location=top.location);
}


aspect production intType
top::Type ::=
{
  top.substituted = intType(location=top.location);

  top.upSubst =
      case top.unifyWith of
      | intType() -> top.downSubst
      | varType(v) -> addSubst(v, top, top.downSubst)
      | _ ->
        addErrSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                    top.pp, top.unifyLoc, top.downSubst)
      end;

  top.freshen = intType(location=top.location);
  top.freshenSubst = emptySubst();

  top.isExtensible = false;
  top.isPC = false;

  top.containsVars = false;
}


aspect production stringType
top::Type ::=
{
  top.substituted = stringType(location=top.location);

  top.upSubst =
      case top.unifyWith of
      | stringType() -> top.downSubst
      | varType(v) -> addSubst(v, top, top.downSubst)
      | _ ->
        addErrSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                    top.pp, top.unifyLoc, top.downSubst)
      end;

  top.freshen = stringType(location=top.location);
  top.freshenSubst = emptySubst();

  top.isExtensible = false;
  top.isPC = false;

  top.containsVars = false;
}


aspect production errorType
top::Type ::=
{
  top.substituted = errorType(location=top.location);

  top.upSubst = top.downSubst;

  top.freshen = errorType(location=top.location);
  top.freshenSubst = emptySubst();

  top.isExtensible = false;
  top.isPC = false;

  top.containsVars = false;
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
   subst, substituted<TypeList>,
   unifyWith<TypeList>, unifyLoc, downSubst, upSubst,
   freshen<TypeList>, freshenSubst,
   pcIndex, foundPC,
   containsVars
occurs on TypeList;

aspect production nilTypeList
top::TypeList ::=
{
  top.substituted = nilTypeList(location=top.location);

  top.freshen = nilTypeList(location=top.location);
  top.freshenSubst = emptySubst();

  top.pcIndex = error("pcIndex on nilTypeList");
  top.foundPC = false;

  top.containsVars = false;

  top.upSubst =
      case top.unifyWith of
      | nilTypeList() -> top.downSubst
      | consTypeList(_, _) ->
        addErrSubst("Cannot unify [" ++ top.pp_comma ++ "] and [" ++
           top.unifyWith.pp_comma ++ "]", top.unifyLoc, top.downSubst)
      end;
}


aspect production consTypeList
top::TypeList ::= t::Type rest::TypeList
{
  t.subst = top.subst;
  rest.subst = top.subst;
  top.substituted =
      consTypeList(t.substituted, rest.substituted,
                   location=top.location);

  local freshenSubstituted::TypeList =
        performSubstitutionTypeList(rest, t.freshenSubst);
  top.freshen =
      consTypeList(t.freshen, freshenSubstituted.freshen,
                   location=top.location);
  top.freshenSubst =
      joinSubst(t.freshenSubst, freshenSubstituted.freshenSubst);

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

  top.containsVars = t.containsVars || rest.containsVars;

  t.downSubst = top.downSubst;
  t.unifyLoc = top.unifyLoc;
  t.unifyWith =
      case top.unifyWith of
      | consTypeList(x, _) -> x
      | _ -> error("Should not access")
      end;
  rest.downSubst = t.upSubst;
  rest.unifyLoc = top.unifyLoc;
  rest.unifyWith =
      case top.unifyWith of
      | consTypeList(_, x) -> x
      | _ -> error("Sholud not access")
      end;
  top.upSubst =
      case top.unifyWith of
      | consTypeList(_, _) -> rest.upSubst
      | nilTypeList() ->
        addErrSubst("Cannot unify [" ++ top.pp_comma ++ "] and [" ++
           top.unifyWith.pp_comma ++ "]", top.unifyLoc, top.downSubst)
      end;
}





type Substitution = Either<[Message] [(String, Type)]>;

function emptySubst
Substitution ::=
{
  return right([]);
}


--Need to be certain
-- 1. varName must not have a binding in base
-- 2. No vars from base occur in ty
function addSubst
Substitution ::= varName::String ty::Type base::Substitution
{
  return
     case base of
     | left(err) -> left(err)
     | right(lst) ->
       right((varName, ty)::
          --replace any occurrences of varName with ty in base
          map(\ p::(String, Type) ->
                (p.1,
                 performSubstitutionType(p.2,
                    right([(varName, ty)]))),
              lst))
     end;
}


--When using joinSubst, you need to be sure
-- 1. No vars from either substitution occur in the types referenced
--    in each other
-- 2. The vars bound in each are disjoint
function joinSubst
Substitution ::= s1::Substitution s2::Substitution
{
  return
     case s1, s2 of
     | left(err1), left(err2) -> left(err1 ++ err2)
     | left(err), right(_) -> left(err)
     | right(_), left(err) -> left(err)
     | right(l1), right(l2) -> right(l1 ++ l2)
     end;
}


function addErrSubst
Substitution ::= err::String loc::Location base::Substitution
{
  return
     case base of
     | left(errs) -> left(errorMessage(err, location=loc)::errs)
     | right(_) -> left([errorMessage(err, location=loc)])
     end;
}


function lookupSubst
Maybe<Type> ::= name::String subst::Substitution
{
  return
     case subst of
     | left(err) -> nothing()
     | right(lst) -> lookup(name, lst)
     end;
}


function errorsFromSubst
[Message] ::= subst::Substitution
{
  return case subst of
         | left(err) -> err
         | _ -> []
         end;
}


function showSubst
String ::= s::Either<[Message] [(String, Type)]>
{
  return
     case s of
     | left(errs) ->
       "Error:  [" ++ implode("; ", map((.pp), errs)) ++ "]"
     | right(lst) ->
       "Subst:  [" ++ 
          implode(", ",
             map(\ p::(String, Type) ->
                   "(" ++ p.1 ++ ", " ++ p.2.pp ++ ")",
                 lst)) ++ "]"
     end;
}





nonterminal TypeUnify with upSubst, downSubst, finalSubst, location;

abstract production typeUnify
top::TypeUnify ::= ty1::Type ty2::Type
{
  local substTy1::Type = performSubstitutionType(ty1, top.downSubst);
  local substTy2::Type = performSubstitutionType(ty2, top.downSubst);
  substTy1.unifyLoc = top.location;
  substTy1.unifyWith = substTy2;
  substTy1.downSubst = top.downSubst;
  top.upSubst = substTy1.upSubst;
}



function performSubstitutionType
Type ::= t::Type s::Substitution
{
  t.subst = s;
  return t.substituted;
}


function performSubstitutionTypeList
TypeList ::= t::TypeList s::Substitution
{
  t.subst = s;
  return t.substituted;
}



function freshenType
Type ::= ty::Type
{
  return ty.freshen;
}


function freshenTypeList
TypeList ::= ty::TypeList
{
  return ty.freshen;
}

