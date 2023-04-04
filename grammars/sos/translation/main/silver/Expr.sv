grammar sos:translation:main:silver;

--for parse
import sos:core:concreteDefs:abstractSyntax;

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
  local letName::String = "let__" ++ toString(genInt());
  top.silverExpr =
      buildLet(letName, "IOVal<" ++ e1.finalType.silverType ++ ">",
         e1.silverExpr,
         case e1.finalType, names of
         | tupleType(tys), _::_::_ ->
           foldr(\ p::(String, Integer, Type) rest::String ->
                   buildLet(p.1, p.3.silverType,
                      letName ++ ".iovalue." ++ toString(p.2), rest),
                 e2.silverExpr,
                 zipWith(pair, names,
                    zipWith(pair, range(1, tys.len + 1), tys.toList)))
         | _, [x] ->
           buildLet(x, e1.finalType.silverType, letName ++ ".iovalue",
                    e2.silverExpr)
         | _, _ -> error("Should not translate")
         end);
  e1.precedingIO = top.precedingIO;
  e2.precedingIO = letName ++ ".io";
}


aspect production seqExpr
top::Expr ::= a::Expr b::Expr
{
  top.silverExpr =
      buildLet(seqName, "IOVal<Unit>", a.silverExpr, b.silverExpr);
  local seqName::String = "seq__" ++ toString(genInt());
  a.precedingIO = top.precedingIO;
  b.precedingIO = seqName ++ ".io";
}


aspect production ifExpr
top::Expr ::= cond::Expr th::Expr el::Expr
{
  top.silverExpr =
      buildLet(condName, "IOVal<Boolean>", cond.silverExpr, ifBody);
  local condName::String = "cond__" ++ toString(genInt());
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
      buildLet(eName, "IOVal<" ++ e.finalType.silverType ++ ">",
               e.silverExpr, buildIOVal(printBody, "unit()"));
  local eName::String = "print_e__" ++ toString(genInt());
  local printBody::String =
      "printT(" ++
      case e.finalType of
      | intType() -> "toInteger(" ++ eName ++ ".iovalue)"
      | stringType() -> eName ++ ".iovalue"
      | nameType(_) -> eName ++ ".iovalue.pp" --Term
      | _ -> error("printExpr.printBody for " ++ e.finalType.pp ++
                   " at " ++ top.location.filename ++ ":" ++
                   toString(top.location.line) ++ ":" ++
                   toString(top.location.column))
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
  local eName::String = "write_e__" ++ toString(genInt());
  local fileName::String = "write_file__" ++ toString(genInt());
  local writeBody::String =
      "writeFileT(" ++ fileName ++ ".iovalue, " ++
      case e.finalType of
      | intType() -> "toInteger(" ++ eName ++ ".iovalue)"
      | stringType() -> eName ++ ".iovalue"
      | nameType(_) -> eName ++ ".iovalue.pp" --Term
      | _ -> error("writeExpr.writeBody for " ++ e.finalType.pp)
      end ++ ", " ++ fileName ++ ".io)";

  e.precedingIO = top.precedingIO;
  file.precedingIO = eName ++ ".io";
}


aspect production readExpr
top::Expr ::= file::Expr
{
  top.silverExpr =
      buildLet(fileName, "IOVal<String>", file.silverExpr, readBody);
  local fileName::String = "read__" ++ toString(genInt());
  local readBody::String =
      "readFileT(" ++ fileName ++ ".iovalue, " ++ fileName ++ ".io)";

  file.precedingIO = top.precedingIO;
}


--vars are the bindings we want out of the judgment
aspect production deriveExpr
top::Expr ::= j::Judgment useVars::[String] vars::[String]
{
  top.silverExpr =
      buildLet(deriveName, "IOVal<Maybe<[(String, Term)]>>",
         funCall, finalResult);
  local deriveName::String = "derive__" ++ toString(genInt());
  local funCall::String =
      "deriveFun__(" ++ j.silver_pp ++ ", " ++ args ++ ", " ++
              top.precedingIO ++ ")";
  local args::String =
      "[" ++ implode(", ",
                map(\ v::String -> "(\"" ++ v ++ "\", " ++ v ++ ")",
                    useVars)) ++ "]";
  local finalResult::String =
      buildIOVal(deriveName ++ ".io",
         "case " ++ deriveName ++ ".iovalue of " ++
         "| nothing() -> (" ++
            implode(", ",
               "false"::map(\ v::String ->
                              "error(\"" ++ v ++ " not defined in " ++
                              "failing derivation\")", vars)) ++
           ") " ++
         "| just(l) -> (" ++
            implode(", ",
               "true"::map(\ v::String ->
                             "lookup(\"" ++ v ++ "\", l).fromJust",
                                     vars)) ++ ") end");
}


--nt is concrete nonterminal name
--varName is name to which we assign the parse result
--parseString is an object-level string to parse
aspect production parseExpr
top::Expr ::= nt::QName parseString::Expr
{
  top.silverExpr =
    buildLet(parseStringName, "IOVal<String>", parseString.silverExpr,
       buildLet(parseName, "IOVal<Either<String Term>>", parseExpr,
          "case " ++ parseName ++ ".iovalue of " ++
          "| right(tm) -> " ++
             buildIOVal(parseName ++ ".io",
                "(true, tm, error(\"Parse succeeded, no errs\"))") ++
         " | left(e) -> " ++
             buildIOVal(parseName ++ ".io",
                "(false, error(e), e)") ++ "end"));

  local parseStringName::String =
      "parseString__" ++ toString(genInt());
  local parseName::String = "parse__" ++ toString(genInt());
  local parseExpr::String =
      "parseFun__(" ++ parseStringName ++ ".iovalue, " ++
             "\"" ++ nt.fullConcreteName.pp ++ "\", " ++
             parseStringName ++ ".io)";

  parseString.precedingIO = top.precedingIO;
}


aspect production orExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<Boolean>", e1.silverExpr,
         "if " ++ e1Name ++ ".iovalue " ++
         "then " ++ buildIOVal(e1Name ++ ".io", "true") ++
        " else " ++ e2.silverExpr);
  local e1Name::String = "or1__" ++ toString(genInt());

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
  local e1Name::String = "and1__" ++ toString(genInt());

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
  local e1Name::String = "lt1__" ++ toString(genInt());
  local e2Name::String = "lt2__" ++ toString(genInt());

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
  local e1Name::String = "gt1__" ++ toString(genInt());
  local e2Name::String = "gt2__" ++ toString(genInt());

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
  local e1Name::String = "le1__" ++ toString(genInt());
  local e2Name::String = "le2__" ++ toString(genInt());

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
  local e1Name::String = "ge1__" ++ toString(genInt());
  local e2Name::String = "ge2__" ++ toString(genInt());

  e1.precedingIO = top.precedingIO;
  e2.precedingIO = e1Name ++ ".io";
}


aspect production eqExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      buildLet(e1Name, "IOVal<" ++ e1.finalType.silverType ++ ">",
               e1.silverExpr,
         buildLet(e2Name, "IOVal<" ++ e2.finalType.silverType ++ ">",
                  e2.silverExpr,
            buildIOVal(e2Name ++ ".io",
               e1Name ++ ".iovalue == " ++ e2Name ++ ".iovalue")));
  local e1Name::String = "eq1__" ++ toString(genInt());
  local e2Name::String = "eq2__" ++ toString(genInt());

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
  local e1Name::String = "plus1__" ++ toString(genInt());
  local e2Name::String = "plus2__" ++ toString(genInt());

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
  local e1Name::String = "minus1__" ++ toString(genInt());
  local e2Name::String = "minus2__" ++ toString(genInt());

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
  local e1Name::String = "mult1__" ++ toString(genInt());
  local e2Name::String = "mult2__" ++ toString(genInt());

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
  local e1Name::String = "div1__" ++ toString(genInt());
  local e2Name::String = "div2__" ++ toString(genInt());

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
  local e1Name::String = "mod1__" ++ toString(genInt());
  local e2Name::String = "mod2__" ++ toString(genInt());

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
  local e1Name::String = "append1__" ++ toString(genInt());
  local e2Name::String = "append2__" ++ toString(genInt());

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


aspect production tupleExpr
top::Expr ::= contents::Args
{
  top.silverExpr =
      foldr(\ p::(String, Type, String) rest::String ->
              buildLet(p.1, "IOVal<" ++ p.2.silverType ++ ">", p.3,
                       rest),
            tuple, contents.silverArgs);
  local tuple::String =
      buildIOVal(contents.resultingIO,
         "(" ++ implode(", ", map(\ p::(String, Type, String) ->
                                    p.1 ++ ".iovalue",
                                  contents.silverArgs)) ++ ")");
  contents.precedingIO = top.precedingIO;
}


aspect production funCall
top::Expr ::= fun::QName args::Args
{
  top.silverExpr =
      foldr(\ p::(String, Type, String) rest::String ->
              buildLet(p.1, "IOVal<" ++ p.2.silverType ++ ">", p.3,
                       rest),
            call, args.silverArgs);
  local funName::String =
      "silverMain:" ++ fun.fullFunction.name.silverFunName;
  local call::String =
      funName ++ "(" ++
         implode(", ", map(\ p::(String, Type, String) ->
                             p.1 ++ ".iovalue",
                           args.silverArgs) ++
                       ["parseFun__", "deriveFun__",
                        args.resultingIO]) ++
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
      buildLet(lName, "IOVal<" ++ l.finalType.silverType ++ ">",
               l.silverExpr,
         buildLet(iName, "IOVal<Integer>", i.silverExpr,
            buildIOVal(iName ++ ".io",
               "head(drop(" ++ iName ++ ".iovalue, " ++ lName ++
                  ".iovalue))")));
  local lName::String = "index1__" ++ toString(genInt());
  local iName::String = "index2__" ++ toString(genInt());

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
  local argName::String = "arg__" ++ toString(genInt());
  top.silverArgs =
      (argName, e.finalType, e.silverExpr)::rest.silverArgs;

  e.precedingIO = top.precedingIO;
  rest.precedingIO = argName ++ ".io";

  top.resultingIO = rest.resultingIO;
}
