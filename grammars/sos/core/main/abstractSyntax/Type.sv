grammar sos:core:main:abstractSyntax;


abstract production boolType
top::Type ::=
{
  top.pp = "bool";

  top.type = top;

  top.isError = false;

  top.isEqual =
      case top.compareTo of
      | boolType() -> true
      | errorType() -> true
      | _ -> false
      end;

  top.substituted = top;
  top.upSubst =
      case top.unifyWith of
      | boolType() -> top.downSubst
      | varType(v) -> addSubst(v, top, top.downSubst)
      | _ ->
       addErrSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                   top.pp, top.unifyLoc, top.downSubst)
      end;

  top.freshen = top;
  top.freshenSubst = emptySubst();

  top.isExtensible = false;
  top.isPC = false;

  top.containsVars = false;

  --similar because it is a constant type
  forwards to intType(location=top.location);
}


abstract production unitType
top::Type ::=
{
  top.pp = "unit";

  top.type = top;

  top.isError = false;

  top.isEqual =
      case top.compareTo of
      | unitType() -> true
      | errorType() -> true
      | _ -> false
      end;

  top.substituted = top;
  top.upSubst =
      case top.unifyWith of
      | unitType() -> top.downSubst
      | varType(v) -> addSubst(v, top, top.downSubst)
      | _ ->
        addErrSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                    top.pp, top.unifyLoc, top.downSubst)
      end;

  top.freshen = top;
  top.freshenSubst = emptySubst();

  top.isExtensible = false;
  top.isPC = false;

  top.containsVars = false;

  --similar-ish because it is a constant type
  forwards to intType(location=top.location);
}


abstract production tupleType
top::Type ::= tys::TypeList
{
  top.pp = "(" ++ tys.pp_comma ++ ")";

  top.type = top;

  tys.tyEnv = top.tyEnv;
  top.isError = tys.isError;

  tys.compareTo =
      case top.compareTo of
      | tupleType(x) -> x
      | _ -> error("Should not access")
      end;
  top.isEqual =
      case top.compareTo of
      | tupleType(_) -> tys.isEqual
      | errorType() -> true
      | _ -> false
      end;

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
  top.freshenSubst = tys.freshenSubst;

  top.isExtensible = false;
  top.isPC = false;

  top.containsVars = tys.containsVars;

  --I don't have a lot of choices
  forwards to nameType(toQName("tuple", top.location),
                       location=top.location);
}


abstract production listType
top::Type ::= ty::Type
{
  top.pp = "[" ++ ty.pp ++ "]";

  top.type = top;

  top.isError = ty.isError;

  ty.tyEnv = top.tyEnv;

  ty.compareTo =
      case top.compareTo of
      | listType(ty2) -> ty2
      | _ -> error("Should not access")
      end;
  top.isEqual =
      case top.compareTo of
      | listType(ty2) -> ty.isEqual
      | errorType() -> true
      | _ -> false
      end;

  top.substituted = listType(ty.substituted, location=top.location);
  ty.subst = top.subst;
  ty.downSubst = top.downSubst;
  ty.unifyLoc = top.unifyLoc;
  ty.unifyWith =
      case top.unifyWith of
      | listType(ty2) -> ty2
      | _ -> error("Should not access")
      end;
  top.upSubst =
      case top.unifyWith of
      | listType(ty2) -> ty.upSubst
      | varType(v) -> addSubst(v, top, top.downSubst)
      | _ ->
        addErrSubst("Cannot unify " ++ top.unifyWith.pp ++ " and " ++
                    top.pp, top.unifyLoc, top.downSubst)
      end;

  --not exactly accurate, but shouldn't show up anywhere else
  forwards to ty;
}
