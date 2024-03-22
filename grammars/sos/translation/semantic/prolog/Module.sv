grammar sos:translation:semantic:prolog;


import sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;
import sos:core:main:abstractSyntax only MainFile;


attribute
   prologProjectionRules, prologRules
occurs on Module, ModuleList;

--Instantiations for just the last module compiled
synthesized attribute
   instanProjPrologRules::[(QName, Maybe<PrologFormula>, PrologTerm)]
occurs on ModuleList;

aspect production stdLibModuleList
top::ModuleList ::= files::Files
{
  top.prologProjectionRules = files.prologProjectionRules;

  top.prologRules = files.prologRules;

  top.instanProjPrologRules = [];
}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.prologProjectionRules =
      m.prologProjectionRules ++ rest.prologProjectionRules;

  top.prologRules = m.prologRules ++ rest.prologRules;

  top.instanProjPrologRules =
     flatMap(
        \ p::(JudgmentEnvItem, [ConstructorEnvItem]) ->
          case lookupBy(\ j1::JudgmentEnvItem j2::JudgmentEnvItem ->
                          j1.name == j2.name,
                        p.1, top.prologProjectionRules) of
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
          | nothing() -> [] --host relations have no projection rule
          end,
        newRuleCombinations);
}




aspect production module
top::Module ::= name::String files::Files
{
  top.prologProjectionRules = files.prologProjectionRules;

  top.prologRules = files.prologRules;
}



attribute
   prologRules, prologProjectionRules
occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.prologProjectionRules = [];
  top.prologRules = [];
}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  top.prologProjectionRules =
      f.prologProjectionRules ++ rest.prologProjectionRules;

  top.prologRules = f.prologRules ++ rest.prologRules;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.prologProjectionRules = rest.prologProjectionRules;

  top.prologRules = rest.prologRules;
}


aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  top.prologProjectionRules = rest.prologProjectionRules;

  top.prologRules = rest.prologRules;
}

