grammar sos:translation:semantic:lambdaProlog;


attribute
   lp<[LambdaPrologDeclaration]>, lpRules, lpTranslationRules_down
occurs on AbsSyntaxDecl;

aspect production initialAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::String constructors::AbsConstructorDecls
{
  top.lp = kindDeclaration(fullName.lpTypeName)::constructors.lp;

  top.lpRules = [];
}


aspect production addAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::QName constructors::AbsConstructorDecls
{
  --kind already declared, just add the new constructors
  top.lp = constructors.lp;

  constructors.lpTranslationRules_down = top.lpTranslationRules_down;
  top.lpRules = constructors.lpRules;
}





attribute
   lp<[LambdaPrologDeclaration]>, lpRules, lpTranslationRules_down
occurs on AbsConstructorDecls;

inherited attribute builtLPType::LambdaPrologType;

aspect production nilAbsConstructorDecls
top::AbsConstructorDecls ::=
{
  top.lp = [];

  top.lpRules = [];
}


aspect production branchAbsConstructorDecls
top::AbsConstructorDecls ::= d1::AbsConstructorDecls
                             d2::AbsConstructorDecls
{
  top.lp = d1.lp ++ d2.lp;

  d1.lpTranslationRules_down = top.lpTranslationRules_down;
  d2.lpTranslationRules_down = top.lpTranslationRules_down;

  top.lpRules = d1.lpRules ++ d2.lpRules;
}


aspect production oneConstructorDecl
top::AbsConstructorDecls ::= name::String tyargs::TypeList
{
  top.lp = [typeDeclaration(fullName.lpConstructorName,
                            foldr(arrowLPType(_, _),
                                  top.builtType.lp, tyargs.types.lp))];

  --Add rules for the judgments from other extensions for new syntax
  --1. get the translation rules from other modules only
  local otherModulesOnly::[(JudgmentEnvItem, String,
                            [LambdaPrologFormula], LambdaPrologTerm)] =
        filter(\ p::(JudgmentEnvItem, String,
                     [LambdaPrologFormula], LambdaPrologTerm) ->
                 !sameModule(top.moduleName, p.1.name),
               top.lpTranslationRules_down);
  --2. relations where the type being built here is the PC
  local thisTypeRelations::[(JudgmentEnvItem, String,
                             [LambdaPrologFormula], LambdaPrologTerm)] =
        filter(\ p::(JudgmentEnvItem, String,
                     [LambdaPrologFormula], LambdaPrologTerm) ->
                 case head(drop(p.1.pcIndex - 1, p.1.types.toList)),
                      top.builtType of
                 | nameType(tyName), nameType(bTyName) -> tyName == bTyName
                 | _, _ -> error("Impossible")
                 end,
               otherModulesOnly);
  --3. produce generic rules with the PC var
  local rulized::[(LambdaPrologRule, String)] =
        map(\ p::(JudgmentEnvItem, String, [LambdaPrologFormula],
                  LambdaPrologTerm) ->
              if null(p.3)
              then (factLambdaPrologRule(p.4), p.2)
              else (ruleLambdaPrologRule(p.4,
                       foldr1(andLambdaPrologFormula, p.3)), p.2),
            thisTypeRelations);
  --4. build a structure for this constructor
  --get vars from all rules so names are fresh in ALL rules
  local usedVars::[String] =
        flatMap(\ p::(LambdaPrologRule, String) ->
                  remove(p.2, p.1.vars),
                rulized);
  local childNames::[String] =
        foldl(\ rest::[String] here::Type ->
                let base::String =
                    case here of
                    | nameType(name) -> capitalize(name.base)
                    | intType() -> "I"
                    | stringType() -> "S"
                    | varType(x) -> capitalize(x)
                    | _ -> "A"
                    end
                in
                  --make each name fresh in ALL the rules
                  freshName(base, rest ++ usedVars)::rest
                end,
              [], tyargs.toList);
  local thisStructure::LambdaPrologTerm =
        foldl(applicationLambdaPrologTerm,
              constLambdaPrologTerm(fullName.lpConstructorName),
              map(varLambdaPrologTerm, childNames));
  --5. replace the variable with the actual structure of this constructor
  local replacedVar::[LambdaPrologRule] =
        map(\ p::(LambdaPrologRule, String) ->
              decorate p.1 with {replaceVar=p.2;
                 replaceVal=thisStructure;}.replaced,
            rulized);
  top.lpRules = replacedVar;
}

