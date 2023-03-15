grammar sos:core:common:abstractSyntax;


inherited attribute subst::Substitution;
synthesized attribute substituted<a>::a;

inherited attribute unifyLoc::Location;
inherited attribute unifyWith<a>::a;

--freshen a type by creating new ty vars for each variable
synthesized attribute freshenSubst::Substitution;
inherited attribute freshenSubst_down::Substitution;
synthesized attribute freshen<a>::a;

--whether there are var types contained within
synthesized attribute containsVars::Boolean;


nonterminal Type with
   pp,
   tyEnv, errors, isError,
   type, --full type (e.g. ty becomes mod:ule:ty)
   subst, substituted<Type>,
   unifyWith<Type>, unifyLoc, downSubst, upSubst,
   freshen<Type>, freshenSubst_down, freshenSubst,
   containsVars,
   location;
propagate errors on Type;

--"Equality" also accepts error type with anything
--Since this is intended for error checking, we assume the error for
--   the error type has already been produced
attribute compareTo, isEqual occurs on Type;

abstract production nameType
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
  top.freshenSubst = top.freshenSubst_down;

  top.pp = name.pp;

  name.tyEnv = top.tyEnv;
  top.errors <- name.tyErrors;
  top.isError = !name.tyFound;

  top.type =
      if name.tyFound
      then name.fullTy
      else errorType(location=top.location);

  top.isEqual =
      case top.compareTo of
      | nameType(x) -> x == name
      | errorType() -> true
      | _ -> false
      end;

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

  --freshening a type variable is checking if it has already been
  --freshened, taking the new one if it has, or creating a new one
  top.freshen =
      case lookupSubst(name, top.freshenSubst_down) of
      | just(ty) -> ty
      | nothing() ->
        varType("_var_" ++ name ++ "_" ++ toString(genInt()),
                location=top.location)
      end;
  top.freshenSubst =
      addSubst(name, top.freshen, top.freshenSubst_down);

  top.type = top;

  top.isError = false;

  top.containsVars = true;

  --This doesn't really fit for forwarding to anything actual
  forwards to errorType(location=top.location);
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
  top.freshenSubst = top.freshenSubst_down;

  top.type = top;

  top.isError = false;

  top.isEqual =
      case top.compareTo of
      | intType() -> true
      | errorType() -> true
      | _ -> false
      end;

  top.containsVars = false;
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
  top.freshenSubst = top.freshenSubst_down;

  top.type = top;

  top.isError = false;

  top.containsVars = false;

  top.isEqual =
      case top.compareTo of
      | stringType() -> true
      | errorType() -> true
      | _ -> false
      end;
}


abstract production listType
top::Type ::= ty::Type
{
  top.pp = "[" ++ ty.pp ++ "]";

  top.substituted = listType(ty.substituted, location=top.location);

  top.upSubst =
      case top.unifyWith of
      | listType(ty2) -> ty.upSubst
      | varType(v) -> addSubst(v, top, top.downSubst)
      | _ ->
        addErrSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                    top.pp, top.unifyLoc, top.downSubst)
      end;

  ty.subst = top.subst;
  ty.downSubst = top.downSubst;

  top.freshen = listType(ty.freshen, location=top.location);
  ty.freshenSubst_down = top.freshenSubst_down;
  top.freshenSubst = ty.freshenSubst;

  top.type = listType(ty.type, location=top.location);

  ty.tyEnv = top.tyEnv;
  top.isError = ty.isError;

  top.containsVars = ty.containsVars;

  ty.unifyLoc = top.unifyLoc;
  ty.unifyWith =
      case top.unifyWith of
      | listType(ty2) -> ty2
      | _ -> error("Should not access")
      end;

  ty.compareTo =
      case top.compareTo of
      | listType(x) -> x
      | _ -> error("Should not access")
      end;
  top.isEqual =
      case top.compareTo of
      | listType(x) -> ty.isEqual
      | errorType() -> true
      | _ -> false
      end;
}


abstract production tupleType
top::Type ::= tys::TypeList
{
  top.pp = "(|" ++ tys.pp_comma ++ "|)";

  tys.subst = top.subst;
  top.substituted = tupleType(tys.substituted, location=top.location);

  tys.downSubst = top.downSubst;
  tys.unifyLoc = top.unifyLoc;
  tys.unifyWith =
      case top.unifyWith of
      | tupleType(t2) -> t2
      | _ -> error("Should not access")
      end;
  top.upSubst =
      case top.unifyWith of
      | tupleType(t2) -> tys.upSubst
      | varType(v) -> addSubst(v, top, top.downSubst)
      | _ ->
        addErrSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                    top.pp, top.unifyLoc, top.downSubst)
      end;

  top.freshen = tupleType(tys.freshen, location=top.location);
  tys.freshenSubst_down = top.freshenSubst_down;
  top.freshenSubst = tys.freshenSubst;

  top.type = tupleType(tys.types, location=top.location);

  tys.tyEnv = top.tyEnv;
  top.isError = tys.isError;

  top.containsVars = tys.containsVars;

  tys.compareTo =
      case top.compareTo of
      | tupleType(x) -> x
      | _ -> error("Should not access")
      end;
  top.isEqual =
      case top.compareTo of
      | tupleType(x) -> tys.isEqual
      | errorType() -> true
      | _ -> false
      end;
}


abstract production errorType
top::Type ::=
{
  top.pp = "<error>";

  top.substituted = errorType(location=top.location);

  top.upSubst = top.downSubst;

  top.freshen = errorType(location=top.location);
  top.freshenSubst = top.freshenSubst_down;

  top.type = top;

  top.isError = true;

  top.containsVars = false;

  top.isEqual = true;
}





nonterminal TypeList with
   pp_comma, pp_space,
   tyEnv, errors, isError,
   subst, substituted<TypeList>,
   unifyWith<TypeList>, unifyLoc, downSubst, upSubst,
   freshen<TypeList>, freshenSubst_down, freshenSubst,
   toList<Type>, len,
   containsVars,
   types, --full types using Type.type
   location;
propagate errors on TypeList;

attribute compareTo, isEqual occurs on TypeList;

abstract production nilTypeList
top::TypeList ::=
{
  top.pp_comma = "";
  top.pp_space = "";

  top.toList = [];
  top.len = 0;

  top.substituted = nilTypeList(location=top.location);

  top.freshen = nilTypeList(location=top.location);
  top.freshenSubst = top.freshenSubst_down;

  top.types = nilTypeList(location=top.location);

  top.containsVars = false;

  top.upSubst =
      case top.unifyWith of
      | nilTypeList() -> top.downSubst
      | consTypeList(_, _) ->
        addErrSubst("Cannot unify [" ++ top.pp_comma ++ "] and [" ++
           top.unifyWith.pp_comma ++ "]", top.unifyLoc, top.downSubst)
      end;

  top.isEqual =
      case top.compareTo of
      | nilTypeList() -> true
      | _ -> false
      end;

  top.isError = false;
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

  top.freshen = consTypeList(t.freshen, rest.freshen,
                             location=top.location);
  t.freshenSubst_down = top.freshenSubst_down;
  rest.freshenSubst_down = t.freshenSubst;
  top.freshenSubst = rest.freshenSubst;

  top.types = consTypeList(t.type, rest.types, location=top.location);

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

  t.compareTo =
    case top.compareTo of
    | consTypeList(x, _) -> x
    | _ -> error("No type")
    end;
  rest.compareTo =
       case top.compareTo of
       | consTypeList(_, x) -> x
       | _ -> error("No type")
       end;
  top.isEqual =
      case top.compareTo of
      | consTypeList(x, _) -> t.isEqual && rest.isEqual
      | _ -> false
      end;

  top.isError = t.isError || rest.isError;
}


function toTypeList
TypeList ::= tys::[Type] loc::Location
{
  return foldr(consTypeList(_, _, location=loc),
               nilTypeList(location=loc), tys);
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


--for when we don't have types to unify, but still need the unify
abstract production blankUnify
top::TypeUnify ::=
{
  top.upSubst = top.downSubst;
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
  ty.freshenSubst_down = emptySubst();
  return ty.freshen;
}


function freshenTypeList
TypeList ::= ty::TypeList
{
  ty.freshenSubst_down = emptySubst();
  return ty.freshen;
}
