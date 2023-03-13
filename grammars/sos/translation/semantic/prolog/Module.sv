grammar sos:translation:semantic:prolog;


import sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;
import sos:core:main:abstractSyntax only MainFile;


attribute
   prologTranslationRules, prologRules
occurs on Module, ModuleList;

--Instantiations for just the last module compiled
synthesized attribute
   instanTransPrologRules::[(QName, Maybe<PrologFormula>, PrologTerm)]
occurs on ModuleList;

aspect production stdLibModuleList
top::ModuleList ::= files::Files
{
  top.prologTranslationRules = files.prologTranslationRules;

  top.prologRules = files.prologRules;

  top.instanTransPrologRules = [];
}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.prologTranslationRules =
      m.prologTranslationRules ++ rest.prologTranslationRules;

  top.prologRules = m.prologRules ++ rest.prologRules;

  top.instanTransPrologRules =
     flatMap(
        \ p::(JudgmentEnvItem, [ConstructorEnvItem]) ->
          case lookupBy(\ j1::JudgmentEnvItem j2::JudgmentEnvItem ->
                          j1.name == j2.name,
                        p.1, top.prologTranslationRules) of
          | just((pc, premises, conclusion)) ->
            map(\ c::ConstructorEnvItem ->
                  let childNames::[String] =
                      map(\ x::Type -> "X" ++ toString(genInt()),
                          c.types.toList)
                  in
                  let tm::PrologTerm =
                      if null(childNames)
                      then constPrologTerm(
                              "constr___" ++ c.name.prolog)
                      else applicationTerm(
                              "constr___" ++ c.name.prolog,
                              foldr(\ x::String
                                      rest::PrologTermList ->
                                      consPrologTermList(
                                         varPrologTerm(x),
                                         rest),
                                    nilPrologTermList(),
                                    childNames))
                  in
                  let newPrem::Maybe<PrologFormula> =
                      case premises of
                      | just(f) ->
                        just(decorate f with {
                                replaceVar = pc;
                                replaceVal = tm;
                             }.replaced)
                      | nothing() -> nothing()
                      end
                  in
                  let newConc::PrologTerm =
                      decorate conclusion with {
                         replaceVar = pc;
                         replaceVal = tm;
                      }.replaced
                  in
                    (p.1.name, newPrem, newConc)
                  end end end end,
                p.2)
          | nothing() -> [] --host relations have no translation rule
          end,
        newRuleCombinations);
}




aspect production module
top::Module ::= name::String files::Files
{
  top.prologTranslationRules = files.prologTranslationRules;

  top.prologRules = files.prologRules;
}



attribute
   prologRules, prologTranslationRules
occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.prologTranslationRules = [];
  top.prologRules = [];
}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  top.prologTranslationRules =
      f.prologTranslationRules ++ rest.prologTranslationRules;

  top.prologRules = f.prologRules ++ rest.prologRules;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.prologTranslationRules = rest.prologTranslationRules;

  top.prologRules = rest.prologRules;
}


aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  top.prologTranslationRules = rest.prologTranslationRules;

  top.prologRules = rest.prologRules;
}

