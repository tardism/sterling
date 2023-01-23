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
      |_ ->
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


abstract production resultType
top::Type ::=
{
  top.pp = "?result";

  top.type = top;

  top.isError = false;

  top.isEqual =
      case top.compareTo of
      | resultType() -> true
      | errorType() -> true
      | _ -> false
      end;

  top.substituted = top;
  top.upSubst =
      case top.unifyWith of
      | resultType() -> top.downSubst
      | varType(v) -> addSubst(v, top, top.downSubst)
      |_ ->
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
