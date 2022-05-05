grammar sos:translation:prolog;


attribute prologTranslationRules_down, prologRules occurs on AbsSyntaxDecl, AbsConstructorDecls;

aspect production initialAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::String constructors::AbsConstructorDecls
{
  --Can't need to instantiate translation rules for newly-declared type
  top.prologRules = [];
}


aspect production addAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::QName constructors::AbsConstructorDecls
{
  constructors.prologTranslationRules_down =
     top.prologTranslationRules_down;
  top.prologRules = constructors.prologRules;
}




aspect production nilAbsConstructorDecls
top::AbsConstructorDecls ::=
{
  top.prologRules = [];
}


aspect production branchAbsConstructorDecls
top::AbsConstructorDecls ::= d1::AbsConstructorDecls
                             d2::AbsConstructorDecls
{
  d1.prologTranslationRules_down = top.prologTranslationRules_down;
  d2.prologTranslationRules_down = top.prologTranslationRules_down;
  top.prologRules = d1.prologRules ++ d2.prologRules;
}


aspect production oneConstructorDecl
top::AbsConstructorDecls ::= name::String tyargs::TypeList
{
  --get the translation rules from other modules only
  local otherModulesOnly::[(JudgmentEnvItem, String,
                            Maybe<PrologFormula>, PrologTerm)] =
        filter(\ p::(JudgmentEnvItem, String,
                     Maybe<PrologFormula>, PrologTerm) ->
                 !sameModule(top.moduleName, p.1.name),
               top.prologTranslationRules_down);
  --relations where the type being built here is the PC
  local thisTypeRelations::[(JudgmentEnvItem, String,
                             Maybe<PrologFormula>, PrologTerm)] =
        filter(\ p::(JudgmentEnvItem, String,
                     Maybe<PrologFormula>, PrologTerm) ->
                 case head(drop(p.1.pcIndex - 1, p.1.types.toList)),
                      top.builtType of
                 | nameType(tyName), nameType(bTyName) -> tyName == bTyName
                 | _, _ -> error("Impossible")
                 end,
               otherModulesOnly);
  --build a structure for this with 
  local thisStructure::PrologTerm =
        applicationTerm(fullName.pp,
           foldr(consPrologTermList, nilPrologTermList(),
                 map(\ x::Type ->
                       varPrologTerm("_" ++name ++ "_fillvar_" ++
                                     toString(genInt())),
                     tyargs.toList)));
  --replace the variable with the actual structure of this constructor
  local replacedVar::[(JudgmentEnvItem, String,
                       Maybe<PrologFormula>, PrologTerm)] =
        map(\ p::(JudgmentEnvItem, String, Maybe<PrologFormula>,
                  PrologTerm) ->
              (p.1, p.2,
               case p.3 of
               | just(f) ->
                 just(decorate f with {replaceVar=p.2;
                         replaceVal=thisStructure;}.replaced)
               | nothing() -> nothing()
               end,
               decorate p.4 with {replaceVar=p.2;
                  replaceVal=thisStructure;}.replaced),
            thisTypeRelations);
  --
  top.prologRules =
      map(\ p::(JudgmentEnvItem, String, Maybe<PrologFormula>,
                PrologTerm) ->
            (p.1.name, p.3, p.4),
          replacedVar);
}

