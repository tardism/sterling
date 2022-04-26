grammar sos:core:files:abstractSyntax;


inherited attribute subst::Substitution;
synthesized attribute substituted<a>::a;

inherited attribute unifyLoc::Location;
inherited attribute unifyWith<a>::a;

--freshen a type by creating new ty vars for each variable
synthesized attribute freshenSubst::Substitution;
synthesized attribute freshen<a>::a;
inherited attribute freshenLoc::Location;


nonterminal Type with
   pp,
   tyEnv, errors,
   subst, substituted<Type>,
   unifyWith<Type>, unifyLoc, downSubst, upSubst,
   freshen<Type>, freshenSubst,
   isExtensible,
   location;
propagate errors on Type;

abstract production nameType
top::Type ::= name::QName
{
  top.pp = name.pp;

  name.tyEnv = top.tyEnv;
  top.errors <- name.tyErrors;

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
}


abstract production intType
top::Type ::=
{
  top.pp = "int";

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
}


abstract production stringType
top::Type ::=
{
  top.pp = "string";

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
}


abstract production errorType
top::Type ::=
{
  top.pp = "<error>";

  top.substituted = errorType(location=top.location);

  top.upSubst = top.downSubst;

  top.freshen = errorType(location=top.location);
  top.freshenSubst = emptySubst();

  top.isExtensible = false;
}





nonterminal TypeList with
   pp_comma, pp_space,
   tyEnv, errors,
   toList<Type>, len,
   subst, substituted<TypeList>,
   freshen<TypeList>, freshenSubst,
   location;
propagate errors on TypeList;

--To check whether the PC is an extensible type, the index of it
--When 0, it should be an extensible type
inherited attribute expectedPCIndex::Maybe<Integer> occurs on TypeList;

abstract production nilTypeList
top::TypeList ::=
{
  top.pp_comma = "";
  top.pp_space = "";

  top.toList = [];
  top.len = 0;

  top.substituted = nilTypeList(location=top.location);

  top.freshen = nilTypeList(location=top.location);
  top.freshenSubst = emptySubst();
}


abstract production consTypeList
top::TypeList ::= t::Type rest::TypeList
{
  top.pp_comma = if rest.pp_comma == ""
                 then t.pp else t.pp ++ ", " ++ rest.pp_comma;
  top.pp_space = if rest.pp_space == ""
                 then t.pp else t.pp ++ " " ++ rest.pp_space;

  t.tyEnv = top.tyEnv;
  rest.tyEnv = top.tyEnv;

  top.toList = t::rest.toList;
  top.len = 1 + rest.len;

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

  rest.expectedPCIndex =
       bind(top.expectedPCIndex, \ x::Integer -> just(x - 1));
  top.errors <-
      case top.expectedPCIndex of
      | just(0) when !t.isExtensible ->
        [errorMessage("Primary component must be an extensible " ++
            "type; found non-extensible type " ++ t.pp,
            location=top.location)]
      | _ -> []
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

