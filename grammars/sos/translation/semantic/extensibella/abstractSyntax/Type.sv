grammar sos:translation:semantic:extensibella:abstractSyntax;


attribute
   eb<ExtensibellaType>, ebIs
occurs on Type;

aspect production nameType
top::Type ::= name::QName
{
  top.eb =
      case name.fullTy of
      | nameType(n) -> extensibellaNameTy(n.ebTypeName)
      | _ -> error("Not possible")
      end;

  top.ebIs = name.ebIsName;
}


aspect production varType
top::Type ::= name::String
{
  --add "Var_" to the beginning to be sure it is capitalized
  top.eb = extensibellaVarTy("Var_" ++ name);

  top.ebIs = error("varType.ebIs");
}


aspect production intType
top::Type ::=
{
  top.eb = extensibellaIntTy();

  top.ebIs = "extensibella-$-stdLib-$-is_integer";
}


aspect production stringType
top::Type ::=
{
  top.eb = extensibellaStringTy();

  top.ebIs = "extensibella-$-stdLib-$-is_string";
}


aspect production tupleType
top::Type ::= tys::TypeList
{
  top.eb = foldr1(extensibellaPairTy, tys.eb);

  top.ebIs =
      foldr1(\ a::String b::String ->
               "extensibella-$-stdLib-$-is_pair (" ++ a ++ ") (" ++ b ++ ")",
             tys.ebIses);
}


aspect production listType
top::Type ::= ty::Type
{
  top.eb = extensibellaListTy(ty.eb);

  top.ebIs = "extensibella-$-stdLib-$-is_list (" ++ ty.ebIs ++ ")";
}


aspect production errorType
top::Type ::=
{
  top.eb = error("Should not translate in presence of errors");

  top.ebIs = error("Should not translate in presence of errors");
}


aspect production pcType
top::Type ::= t::Type
{
  --get by copying from forward
}





attribute
   eb<[ExtensibellaType]>, ebIses
occurs on TypeList;

synthesized attribute ebIses::[String];

aspect production nilTypeList
top::TypeList ::=
{
  top.eb = [];

  top.ebIses = [];
}


aspect production consTypeList
top::TypeList ::= t::Type rest::TypeList
{
  top.eb = t.eb::rest.eb;

  top.ebIses = t.ebIs::rest.ebIses;
}
