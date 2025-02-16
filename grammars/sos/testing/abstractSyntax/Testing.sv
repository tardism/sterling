grammar sos:testing:abstractSyntax;

imports sos:core:common:abstractSyntax;
imports sos:core:semanticDefs:abstractSyntax;
imports sos:core:concreteDefs:abstractSyntax;
imports sos:core:main:abstractSyntax;


{--------------------------------------------------------------------
                            SEMANTIC FILES
 --------------------------------------------------------------------}
abstract production errorExpectedDecls
top::Decls ::= err::String decls::Decls
{
  decls.moduleName = top.moduleName;

  decls.tyEnv = top.tyEnv;
  decls.constructorEnv = top.constructorEnv;
  decls.judgmentEnv = top.judgmentEnv;
  decls.projectionEnv = top.projectionEnv;
  decls.ruleEnv = top.ruleEnv;

  decls.projRuleConstructors_down = top.projRuleConstructors_down;

  --Don't want errors from decls propagating automatically
  top.errors := [];

  --Partition into ([errors matching err], [errors not matching err])
  local parts::([Message], [Message]) =
        partition(\ m::Message ->
                    case m of
                    | errorMessage(text) -> indexOf(err, text) > -1
                    | _ -> false
                    end, decls.errors);

  --Must find at least one error containing err for success
  top.errors <-
      case parts.1 of
      | [] ->
        [errorMessage("No error containing \"" ++ err ++ "\" found",
                      location=top.location)]
      | _ -> []
      end;
  --Anything else goes straight into the errors
  top.errors <- parts.2;

  forwards to ^decls;
}


abstract production warningExpectedDecls
top::Decls ::= wrn::String decls::Decls
{
  decls.moduleName = top.moduleName;

  decls.tyEnv = top.tyEnv;
  decls.constructorEnv = top.constructorEnv;
  decls.judgmentEnv = top.judgmentEnv;
  decls.projectionEnv = top.projectionEnv;
  decls.ruleEnv = top.ruleEnv;

  decls.projRuleConstructors_down = top.projRuleConstructors_down;

  --Don't want errors from decls propagating automatically
  top.errors := [];

  --Partition into ([errors matching err], [errors not matching err])
  local parts::([Message], [Message]) =
        partition(\ m::Message ->
                    case m of
                    | warningMessage(text) -> indexOf(wrn, text) > -1
                    | _ -> false
                    end, decls.errors);

  --Must find at least one warning containing wrn for success
  top.errors <-
      case parts.1 of
      | [] ->
        [errorMessage("No warning containing \"" ++ wrn ++ "\" found",
                      location=top.location)]
      | _ -> []
      end;
  --Anything else goes straight into the errors
  top.errors <- parts.2;

  forwards to ^decls;
}





{--------------------------------------------------------------------
                            CONCRETE FILES
 --------------------------------------------------------------------}
abstract production errorExpectedConcreteDecls
top::ConcreteDecls ::= err::String decls::ConcreteDecls
{
  decls.moduleName = top.moduleName;

  decls.concreteEnv = top.concreteEnv;
  decls.constructorEnv = top.constructorEnv;
  decls.tyEnv = top.tyEnv;

  --Don't want errors from decls propagating automatically
  top.errors := [];

  --Partition into ([errors matching err], [errors not matching err])
  local parts::([Message], [Message]) =
        partition(\ m::Message ->
                    case m of
                    | errorMessage(text) -> indexOf(err, text) > -1
                    | _ -> false
                    end, decls.errors);

  --Must find at least one error containing err for success
  top.errors <-
      case parts.1 of
      | [] ->
        [errorMessage("No error containing \"" ++ err ++ "\" found",
                      location=top.location)]
      | _ -> []
      end;
  --Anything else goes straight into the errors
  top.errors <- parts.2;

  forwards to ^decls;
}


abstract production warningExpectedConcreteDecls
top::ConcreteDecls ::= wrn::String decls::ConcreteDecls
{
  decls.moduleName = top.moduleName;

  decls.concreteEnv = top.concreteEnv;
  decls.constructorEnv = top.constructorEnv;
  decls.tyEnv = top.tyEnv;

  --Don't want errors from decls propagating automatically
  top.errors := [];

  --Partition into ([errors matching err], [errors not matching err])
  local parts::([Message], [Message]) =
        partition(\ m::Message ->
                    case m of
                    | warningMessage(text) -> indexOf(wrn, text) > -1
                    | _ -> false
                    end, decls.errors);

  --Must find at least one warning containing wrn for success
  top.errors <-
      case parts.1 of
      | [] ->
        [errorMessage("No warning containing \"" ++ wrn ++ "\" found",
                      location=top.location)]
      | _ -> []
      end;
  --Anything else goes straight into the errors
  top.errors <- parts.2;

  forwards to ^decls;
}





{--------------------------------------------------------------------
                              MAIN FILES
 --------------------------------------------------------------------}
abstract production errorExpectedMainDecls
top::MainDecls ::= err::String decls::MainDecls
{
  decls.moduleName = top.moduleName;

  decls.concreteEnv = top.concreteEnv;
  decls.constructorEnv = top.constructorEnv;
  decls.tyEnv = top.tyEnv;
  decls.judgmentEnv = top.judgmentEnv;
  decls.projectionEnv = top.projectionEnv;
  decls.funEnv = top.funEnv;

  --Don't want errors from decls propagating automatically
  top.errors := [];

  --Partition into ([errors matching err], [errors not matching err])
  local parts::([Message], [Message]) =
        partition(\ m::Message ->
                    case m of
                    | errorMessage(text) -> indexOf(err, text) > -1
                    | _ -> false
                    end, decls.errors);

  --Must find at least one error containing err for success
  top.errors <-
      case parts.1 of
      | [] ->
        [errorMessage("No error containing \"" ++ err ++ "\" found",
                      location=top.location)]
      | _ -> []
      end;
  --Anything else goes straight into the errors
  top.errors <- parts.2;

  forwards to ^decls;
}


abstract production warningExpectedMainDecls
top::MainDecls ::= wrn::String decls::MainDecls
{
  decls.moduleName = top.moduleName;

  decls.concreteEnv = top.concreteEnv;
  decls.constructorEnv = top.constructorEnv;
  decls.tyEnv = top.tyEnv;
  decls.judgmentEnv = top.judgmentEnv;
  decls.projectionEnv = top.projectionEnv;
  decls.funEnv = top.funEnv;

  --Don't want errors from decls propagating automatically
  top.errors := [];

  --Partition into ([errors matching err], [errors not matching err])
  local parts::([Message], [Message]) =
        partition(\ m::Message ->
                    case m of
                    | warningMessage(text) -> indexOf(wrn, text) > -1
                    | _ -> false
                    end, decls.errors);

  --Must find at least one warning containing wrn for success
  top.errors <-
      case parts.1 of
      | [] ->
        [errorMessage("No warning containing \"" ++ wrn ++ "\" found",
                      location=top.location)]
      | _ -> []
      end;
  --Anything else goes straight into the errors
  top.errors <- parts.2;

  forwards to ^decls;
}
