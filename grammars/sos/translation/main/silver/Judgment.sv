grammar sos:translation:main:silver;

synthesized attribute silver_pp::String;

attribute
   silver_pp
occurs on Judgment;

aspect production relation
top::Judgment ::= rel::QName args::TermList
{
  top.silver_pp =
      "relation(" ++ rel.fullJudgment.name.silver_pp ++ ", " ++
                args.silver_pp ++ ", location=bogusLoc())";
}


aspect production negationRelation
top::Judgment ::= rel::QName args::TermList
{
  top.silver_pp =
     "negationRelation(" ++ rel.fullJudgment.name.silver_pp ++ ", " ++
                       args.silver_pp ++ ", location=bogusLoc())";
}


aspect production projJudgment
top::Judgment ::= args::TermList ty::QName t::Term projection::Term
{
  top.silver_pp =
      case ty.fullTy of
      | nameType(n) ->
        "projJudgment(" ++ args.silver_pp ++ ", " ++
            n.silver_pp ++ ", " ++ t.silver_pp ++ ", " ++
            projection.silver_pp ++ ", location=bogusLoc())"
      | _ -> error("Should not access")
      end;
}


aspect production binOpJudgment
top::Judgment ::= t1::Term op::BinOp t2::Term result::Term
{
  top.silver_pp =
      "binOpJudgment(" ++ t1.silver_pp ++ ", " ++
          op.silver_pp ++ ", " ++ t2.silver_pp ++ ", " ++
          result.silver_pp ++ ", location=bogusLoc())";
}


aspect production topBinOpJudgment
top::Judgment ::= t1::Term op::TopBinOp t2::Term
{
  top.silver_pp =
      "topBinOpJudgment(" ++ t1.silver_pp ++ ", " ++
          op.silver_pp ++ ", " ++ t2.silver_pp ++
          ", location=bogusLoc())";
}





attribute
   silver_pp
occurs on BinOp;

aspect production plusOp
top::BinOp ::=
{
  top.silver_pp = "plusOp(location=bogusLoc())";
}


aspect production minusOp
top::BinOp ::=
{
  top.silver_pp = "minusOp(location=bogusLoc())";
}


aspect production multOp
top::BinOp ::=
{
  top.silver_pp = "multOp(location=bogusLoc())";
}


aspect production divOp
top::BinOp ::=
{
  top.silver_pp = "divOp(location=bogusLoc())";
}


aspect production modOp
top::BinOp ::=
{
  top.silver_pp = "modOp(location=bogusLoc())";
}


aspect production appendOp
top::BinOp ::=
{
  top.silver_pp = "appendOp(location=bogusLoc())";
}





attribute
   silver_pp
occurs on TopBinOp;

aspect production eqOp
top::TopBinOp ::=
{
  top.silver_pp = "eqOp(location=bogusLoc())";
}


aspect production neqOp
top::TopBinOp ::=
{
  top.silver_pp = "neqOp(location=bogusLoc())";
}


aspect production lessOp
top::TopBinOp ::=
{
  top.silver_pp = "lessOp(location=bogusLoc())";
}


aspect production greaterOp
top::TopBinOp ::=
{
  top.silver_pp = "greaterOp(location=bogusLoc())";
}


aspect production leqOp
top::TopBinOp ::=
{
  top.silver_pp = "leqOp(location=bogusLoc())";
}


aspect production geqOp
top::TopBinOp ::=
{
  top.silver_pp = "geqOp(location=bogusLoc())";
}





attribute
   silver_pp
occurs on Term;

aspect production const
top::Term ::= name::QName
{
  top.silver_pp =
      "const(" ++ name.fullConstrName.silver_pp ++
          ", location=bogusLoc())";
}


aspect production var
top::Term ::= name::String
{
  top.silver_pp = "var(\"" ++ name ++ "\", location=bogusLoc())";
}


aspect production num
top::Term ::= int::Integer
{
  top.silver_pp = "num(" ++ toString(int) ++ ", location=bogusLoc())";
}


aspect production stringConst
top::Term ::= s::String
{
  top.silver_pp = "stringConst(\"" ++ s ++ "\", location=bogusLoc())";
}


aspect production appTerm
top::Term ::= constructor::QName args::TermList
{
  top.silver_pp =
      "appTerm(" ++ constructor.fullConstrName.silver_pp ++ ", " ++
               args.silver_pp ++ ", location=bogusLoc())";
}


aspect production tupleTerm
top::Term ::= contents::TermList
{
  top.silver_pp = "tupleTerm(" ++ contents.silver_pp ++
                          ", location=bogusLoc())";
}


aspect production nilTerm
top::Term ::=
{
  top.silver_pp = "nilTerm(location=bogusLoc())";
}


aspect production consTerm
top::Term ::= hd::Term tl::Term
{
  top.silver_pp =
      "consTerm(" ++ hd.silver_pp ++ ", " ++ tl.silver_pp ++
             ", location=bogusLoc())";
}


aspect production ascriptionTerm
top::Term ::= tm::Term ty::Type
{
  top.silver_pp = tm.silver_pp;
}





attribute
   silver_pp
occurs on TermList;

aspect production nilTermList
top::TermList ::=
{
  top.silver_pp = "nilTermList(location=bogusLoc())";
}


aspect production consTermList
top::TermList ::= t::Term rest::TermList
{
  top.silver_pp =
      "consTermList(" ++ t.silver_pp ++ ", " ++ rest.silver_pp ++
                 ", location=bogusLoc())";
}





attribute
   silver_pp
occurs on QName;

aspect production baseName
top::QName ::= name::String
{
  top.silver_pp = "baseName(\"" ++ name ++ "\", location=bogusLoc())";
}


aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  top.silver_pp =
      "moduleLayerName(\"" ++ name ++ "\", " ++ rest.silver_pp ++
                    ", location=bogusLoc())";
}
