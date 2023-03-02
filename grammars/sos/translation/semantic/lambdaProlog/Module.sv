grammar sos:translation:semantic:lambdaProlog;

imports sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;
import sos:core:main:abstractSyntax only MainFile;

attribute lpDecls, lpRules, lpTranslationRules occurs on ModuleList;

aspect production nilModuleList
top::ModuleList ::=
{
  top.lpDecls = [];
  top.lpRules = [];
  top.lpTranslationRules = [];
}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.lpDecls = m.lpDecls;
  top.lpRules = m.lpRules ++ instantiatedTranslationRules;
  top.lpTranslationRules =
      m.lpTranslationRules ++ rest.lpTranslationRules;

  local instantiatedTranslationRules::[LambdaPrologRule] =
     flatMap(
        \ p::(JudgmentEnvItem, [ConstructorEnvItem]) ->
          case lookupBy(\ j1::JudgmentEnvItem j2::JudgmentEnvItem ->
                          j1.name == j2.name,
                        p.1, top.lpTranslationRules) of
          | just((pc, r)) ->
            let r_vars::[String] =
                remove(pc, r.vars)
            in
              map(\ c::ConstructorEnvItem ->
                    let childNames::[String] =
                        foldr(\ x::Type rest::[String] ->
                                case x of
                                | nameType(n) ->
                                  freshName(capitalize(n.base),
                                            rest ++ r_vars)
                                | intType() ->
                                  freshName("I", rest ++ r_vars)
                                | stringType() ->
                                  freshName("S", rest ++ r_vars)
                                | tupleType(l) ->
                                  if l.len == 2
                                  then freshName("P", rest ++ r_vars)
                                  else freshName("T", rest ++ r_vars)
                                | listType(ty) ->
                                  freshName("L", rest ++ r_vars)
                                | _ ->
                                  freshName("X", rest ++ r_vars)
                                end::rest,
                              [], c.types.toList)
                    in
                    let tm::LambdaPrologTerm =
                        foldl(applicationLambdaPrologTerm,
                              constLambdaPrologTerm(
                                 c.name.lpConstructorName),
                              map(varLambdaPrologTerm, childNames))
                    in
                      decorate r with {
                         replaceVar = pc;
                         replaceVal = tm;
                      }.replaced
                    end end,
                  p.2)
            end
          | nothing() -> [] --host relations have no translation rule
          end,
        newRuleCombinations);
}





attribute
   lpDecls, lpRules, lpTranslationRules
occurs on Module;

aspect production module
top::Module ::= name::String files::Files
{
  top.lpDecls = files.lpDecls;
  top.lpRules = files.lpRules;
  top.lpTranslationRules = files.lpTranslationRules;
}





attribute
   lpDecls, lpRules, lpTranslationRules
occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.lpDecls = [];

  top.lpRules = [];
  top.lpTranslationRules = [];
}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  top.lpDecls = f.lpDecls ++ rest.lpDecls;

  top.lpRules = f.lpRules ++ rest.lpRules;
  top.lpTranslationRules =
      f.lpTranslationRules ++ rest.lpTranslationRules;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.lpDecls = rest.lpDecls;

  top.lpRules = rest.lpRules;
  top.lpTranslationRules = rest.lpTranslationRules;
}


aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  top.lpDecls = rest.lpDecls;

  top.lpRules = rest.lpRules;
  top.lpTranslationRules = rest.lpTranslationRules;
}
