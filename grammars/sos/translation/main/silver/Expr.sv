grammar sos:translation:main:silver;

attribute
   precedingIO, silverExpr
occurs on Expr;

--text of an equivalent Silver expression
--always has type IOVal<_>
synthesized attribute silverExpr::String;

--Silver expression for the last IO done before this
inherited attribute precedingIO::String;

aspect production letExpr
top::Expr ::= names::[String] e1::Expr e2::Expr
{
  local letName::String =
      if length(names) > 1
      then "_let_" ++ toString(genInt())
      else head(names);
  top.silverExpr =
      buildLet(letName, "IOVal<" ++ e1.type.silverType ++ ">",
         e1.silverExpr,
         case e1.type, names of
         | tupleType(tys), _::_::_ ->
           foldr(\ p::(String, Integer, Type) rest::String ->
                   buildLet(p.1, p.3.silverType,
                      letName ++ ".iovalue." ++ toString(p.2), rest),
                 e2.silverExpr,
                 zipWith(pair, names,
                    zipWith(pair, range(1, tys.len + 1), tys.toList)))
         | _, _ -> e2.silverExpr
         end);
  e1.precedingIO = top.precedingIO;
  e2.precedingIO = letName ++ ".io";
}


aspect production seqExpr
top::Expr ::= a::Expr b::Expr
{
  top.silverExpr =
      buildLet(seqName, "IOVal<Unit>", a.silverExpr, b.silverExpr);
  local seqName::String = "_seq_" ++ toString(genInt());
  a.precedingIO = top.precedingIO;
  b.precedingIO = seqName ++ ".io";
}


aspect production ifExpr
top::Expr ::= cond::Expr th::Expr el::Expr
{
  top.silverExpr =
      buildLet(condName, "IOVal<Boolean>", cond.silverExpr, ifBody);
  local condName::String = "_cond_" ++ toString(genInt());
  local ifBody::String =
      "if " ++ condName ++ ".iovalue" ++ " then " ++ th.silverExpr ++
      " else " ++ el.silverExpr;

  cond.precedingIO = top.precedingIO;
  th.precedingIO = condName ++ ".io";
  el.precedingIO = condName ++ ".io";
}


aspect production printExpr
top::Expr ::= e::Expr
{
  top.silverExpr =
      buildLet(eName, "IOVal<" ++ e.type.silverType ++ ">",
               e.silverExpr, buildIOVal(printBody, "unit()"));
  local eName::String = "_print_e_" ++ toString(genInt());
  local printBody::String =
      "printT(" ++
      case e.type of
      | intType() -> "toInteger(" ++ eName ++ ".iovalue)"
      | stringType() -> eName ++ ".iovalue"
      | nameType(_) -> eName ++ ".iovalue.pp" --Term
      | _ -> error("printExpr.printBody for " ++ e.type.pp)
      end ++ ", " ++ eName ++ ".io)";

  e.precedingIO = top.precedingIO;
}


aspect production writeExpr
top::Expr ::= e::Expr file::Expr
{
  top.silverExpr =
      buildLet(eName, "IOVal<String>", e.silverExpr,
         buildLet(fileName, "IOVal<String>", file.silverExpr,
            buildIOVal(writeBody, "unit()")));
  local eName::String = "_write_e_" ++ toString(genInt());
  local fileName::String = "_write_file_" ++ toString(genInt());
  local writeBody::String =
      "writeFileT(" ++ fileName ++ ".iovalue, " ++
      case e.type of
      | intType() -> "toInteger(" ++ eName ++ ".iovalue)"
      | stringType() -> eName ++ ".iovalue"
      | nameType(_) -> eName ++ ".iovalue.pp" --Term
      | _ -> error("writeExpr.writeBody for " ++ e.type.pp)
      end ++ ", " ++ fileName ++ ".io)";

  e.precedingIO = top.precedingIO;
  file.precedingIO = eName ++ ".io";
}


aspect production readExpr
top::Expr ::= file::Expr
{
  top.silverExpr =
      buildLet(fileName, "IOVal<String>", file.silverExpr, readBody);
  local fileName::String = "_read_" ++ toString(genInt());
  local readBody::String =
      "readFileT(" ++ fileName ++ ".iovalue, " ++ fileName ++ ".io)";

  file.precedingIO = top.precedingIO;
}


--vars are the bindings we want out of the judgment
aspect production deriveExpr
top::Expr ::= j::Judgment vars::[String]
{
  top.silverExpr = error("deriveExpr.silverExpr");
}


--nt is concrete nonterminal name
--varName is name to which we assign the parse result
--parseString is an object-level string to parse
aspect production parseExpr
top::Expr ::= nt::QName parseString::Expr
{
  top.silverExpr = error("parseExpr.silverExpr");
}

aspect production orExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Boolean>", e1.silverExpr,
         "if " ++ e1Name ++ ".iovalue " ++
         "then " ++ buildIOVal(e1Name ++ ".io", "true") ++
        " else " ++ e2.silverExpr);
  local e1Name::String = "_or1_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production andExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Boolean>", e1.silverExpr,
         "if " ++ e1Name ++ ".iovalue " ++
         "then " ++ e2.silverExpr ++
        " else " ++ buildIOVal(e1Name ++ ".io", "true"));
  local e1Name::String = "_and1_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production ltExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Integer>", e1.silverExpr,
         buildLet(e2Name, "IOVal<Integer>", e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue < " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "_lt1_" ++ toString(genInt());
  local e2Name::String = "_lt2_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production gtExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Integer>", e1.silverExpr,
         buildLet(e2Name, "IOVal<Integer>", e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue > " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "_gt1_" ++ toString(genInt());
  local e2Name::String = "_gt2_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production leExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Integer>", e1.silverExpr,
         buildLet(e2Name, "IOVal<Integer>", e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue <= " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "_le1_" ++ toString(genInt());
  local e2Name::String = "_le2_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production geExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Integer>", e1.silverExpr,
         buildLet(e2Name, "IOVal<Integer>", e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue >= " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "_ge1_" ++ toString(genInt());
  local e2Name::String = "_ge2_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production eqExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<" ++ e1.type.silverType ++ ">",
               e1.silverExpr,
         buildLet(e2Name, "IOVal<" ++ e2.type.silverType ++ ">",
                  e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue == " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "_eq1_" ++ toString(genInt());
  local e2Name::String = "_eq2_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production plusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Integer>", e1.silverExpr,
         buildLet(e2Name, "IOVal<Integer>", e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue + " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "_plus1_" ++ toString(genInt());
  local e2Name::String = "_plus2_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production minusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Integer>", e1.silverExpr,
         buildLet(e2Name, "IOVal<Integer>", e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue - " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "_minus1_" ++ toString(genInt());
  local e2Name::String = "_minus2_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production multExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Integer>", e1.silverExpr,
         buildLet(e2Name, "IOVal<Integer>", e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue * " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "_mult1_" ++ toString(genInt());
  local e2Name::String = "_mult2_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production divExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Integer>", e1.silverExpr,
         buildLet(e2Name, "IOVal<Integer>", e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue / " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "_div1_" ++ toString(genInt());
  local e2Name::String = "_div2_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production modExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Integer>", e1.silverExpr,
         buildLet(e2Name, "IOVal<Integer>", e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue % " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "_mod1_" ++ toString(genInt());
  local e2Name::String = "_mod2_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production appendExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<String>", e1.silverExpr,
         buildLet(e2Name, "IOVal<String>", e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue ++ " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "_append1_" ++ toString(genInt());
  local e2Name::String = "_append2_" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production varExpr
top::Expr ::= name::String
{
  top.silverExpr = buildIOVal(top.precedingIO, name);
}


aspect production intExpr
top::Expr ::= i::Integer
{
  top.silverExpr = buildIOVal(top.precedingIO, toString(i));
}


aspect production stringExpr
top::Expr ::= s::String
{
  top.silverExpr = buildIOVal(top.precedingIO, "\"" ++ s ++ "\"");
}


aspect production funCall
top::Expr ::= fun::QName args::Args
{
  top.silverExpr =
      foldr(\ p::(String, Type, String) rest::String ->
              buildLet(p.1, "IOVal<" ++ p.2.silverType ++ ">", p.3,
                       rest),
            call, args.silverArgs);
  local funName::String = fun.fullFunction.name.silverFunName;
  local call::String =
      funName ++ "(" ++
         implode(", ", map(\ p::(String, Type, String) ->
                             p.1 ++ ".iovalue",
                           args.silverArgs) ++ [args.resultingIO]) ++
         ")";
  args.precedingIO = top.precedingIO;
}


aspect production trueExpr
top::Expr ::=
{
  top.silverExpr = buildIOVal(top.precedingIO, "true");
}


aspect production falseExpr
top::Expr ::=
{
  top.silverExpr = buildIOVal(top.precedingIO, "false");
}


aspect production listIndexExpr
top::Expr ::= l::Expr i::Expr
{
  top.silverExpr =
      buildLet(lName, "IOVal<" ++ l.type.silverType ++ ">",
               l.silverExpr,
         buildLet(iName, "IOVal<Integer>", i.silverExpr,
            buildIOVal(iName ++ ".io",
               "head(drop(" ++ iName ++ ".iovalue, " ++ lName ++
                  ".iovalue))")));
  local lName::String = "_index1_" ++ toString(genInt());
  local iName::String = "_index2_" ++ toString(genInt());

  l.precedingIO = top.precedingIO;
  i.precedingIO = lName ++ ".io";
}





attribute
   silverArgs, precedingIO, resultingIO
occurs on Args;

--[(name, SOS-Ext type, expression to assign to that name)]
synthesized attribute silverArgs::[(String, Type, String)];

--IO after computing all the args
synthesized attribute resultingIO::String;

aspect production nilArgs
top::Args ::=
{
  top.silverArgs = [];
  top.resultingIO = top.precedingIO;
}


aspect production consArgs
top::Args ::= e::Expr rest::Args
{
  local argName::String = "_arg_" ++ toString(genInt());
  top.silverArgs = (argName, e.type, e.silverExpr)::rest.silverArgs;

  e.precedingIO = top.precedingIO;
  rest.precedingIO = argName ++ ".io";

  top.resultingIO = rest.resultingIO;
}
