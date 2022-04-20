grammar sos:core:abstractSyntax;


synthesized attribute substituted<a>::a;

inherited attribute unifyWith<a>::a;
synthesized attribute unify::Substitution;

--freshen a type by creating new ty vars for each variable
synthesized attribute freshenSubst::Substitution;
synthesized attribute freshen<a>::a;


nonterminal Type with
   pp,
   tyEnv, errors,
   downSubst, substituted<Type>,
   unifyWith<Type>, unify,
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

  top.unify =
      case top.unifyWith of
      | nameType(n) when n == name -> emptySubst()
      | nameType(n) ->
        errSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                 top.pp, top.location)
      | varType(v) -> varSubst(v, top)
      | _ ->
        errSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                 top.pp, top.location)
      end;

  top.freshen = top;
  top.freshenSubst = emptySubst();

  --assume all name types are extensible
  top.isExtensible = true;
}


abstract production varType
top::Type ::= name::String
{
  top.pp = name;

  top.substituted =
      case lookupSubst(name, top.downSubst) of
      | nothing() -> varType(name, location=top.location)
      | just(ty) -> ty
      end;

  top.unify =
      case top.unifyWith of
      | varType(v) when v == name -> emptySubst()
      | ty -> varSubst(name, ty)
      end;

  top.freshen =
      varType("_var" ++ toString(genInt()), location=top.location);
  top.freshenSubst = varSubst(name, top.freshen);

  --assume variable types are not extensible, as undetermined
  top.isExtensible = false;
}


abstract production intType
top::Type ::=
{
  top.pp = "int";

  top.substituted = intType(location=top.location);

  top.unify =
      case top.unifyWith of
      | intType() -> emptySubst()
      | varType(v) -> varSubst(v, top)
      | _ ->
        errSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                 top.pp, top.location)
      end;

  top.freshen = top;
  top.freshenSubst = emptySubst();

  top.isExtensible = false;
}


abstract production stringType
top::Type ::=
{
  top.pp = "string";

  top.substituted = stringType(location=top.location);

  top.unify =
      case top.unifyWith of
      | stringType() -> emptySubst()
      | varType(v) -> varSubst(v, top)
      | _ ->
        errSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                 top.pp, top.location)
      end;

  top.freshen = top;
  top.freshenSubst = emptySubst();

  top.isExtensible = false;
}


abstract production errorType
top::Type ::=
{
  top.pp = "<error>";

  top.substituted = errorType(location=top.location);

  top.unify = emptySubst();

  top.freshen = top;
  top.freshenSubst = emptySubst();

  top.isExtensible = false;
}





nonterminal TypeList with
   pp_comma, pp_space,
   tyEnv, errors,
   toList<Type>, len,
   downSubst, substituted<TypeList>,
   unifyWith<TypeList>, unify,
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

  top.unify =
      case top.unifyWith of
      | nilTypeList() -> emptySubst()
      | _ ->
        errSubst("Cannot unify empty type list with a non-empty " ++
                 "type list", top.location)
      end;

  top.freshen = top;
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

  t.downSubst = top.downSubst;
  rest.downSubst = top.downSubst;
  top.substituted =
      consTypeList(t.substituted, rest.substituted,
                   location=top.location);

  top.unify =
      case top.unifyWith of
      | consTypeList(ty, r) ->
        let s::Substitution = unifyTypes(t, t)
        in
          joinSubst(s,
             unifyTypeLists(performSubstitutionTypeList(rest, s),
                            performSubstitutionTypeList(r, s)))
        end
      | nilTypeList() ->
        errSubst("Cannot unify empty type list with a non-empty " ++
                 "type list", top.location)
      end;

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


function varSubst
Substitution ::= varName::String ty::Type
{
  return right([(varName, ty)]);
}


function errSubst
Substitution ::= err::String loc::Location
{
  return left([errorMessage(err, location=loc)]);
}


function joinSubst
Substitution ::= s1::Substitution s2::Substitution
{
  return
     case s1, s2 of
     | left(e1), left(e2) -> left(e1 ++ e2)
     | left(e1), _ -> left(e1)
     | _, left(e2) -> left(e2)
     | right(l1), right(l2) -> right(l1 ++ l2)
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





nonterminal TypeUnify with upSubst, downSubst, finalSubst;

abstract production typeUnify
top::TypeUnify ::= ty1::Type ty2::Type
{
  top.upSubst =
      joinSubst(top.downSubst,
         unifyTypes(performSubstitutionType(ty1, top.downSubst),
                    performSubstitutionType(ty2, top.downSubst)));
}


abstract production typeListUnify
top::TypeUnify ::= t1::TypeList t2::TypeList
{
  top.upSubst =
      joinSubst(top.downSubst,
        unifyTypeLists(performSubstitutionTypeList(t1, top.downSubst),
           performSubstitutionTypeList(t2, top.downSubst)));
}



function unifyTypes
Substitution ::= t1::Type t2::Type
{
  t1.unifyWith = t2;
  return t1.unify;
}


function unifyTypeLists
Substitution ::= t1::TypeList t2::TypeList
{
  t1.unifyWith = t2;
  return t1.unify;
}


function performSubstitutionType
Type ::= t::Type s::Substitution
{
  t.downSubst = s;
  return t.substituted;
}


function performSubstitutionTypeList
TypeList ::= t::TypeList s::Substitution
{
  t.downSubst = s;
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

