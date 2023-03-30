grammar sos:testing:abstractSyntax;

imports sos:core:common:abstractSyntax;
imports sos:core:semanticDefs:abstractSyntax;


abstract production errorExpectedDecls
top::Decls ::= err::String decls::Decls
{
  decls.moduleName = top.moduleName;

  decls.tyEnv = top.tyEnv;
  decls.constructorEnv = top.constructorEnv;
  decls.judgmentEnv = top.judgmentEnv;
  decls.translationEnv = top.translationEnv;
  decls.ruleEnv = top.ruleEnv;

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

  forwards to decls;
}


abstract production warningExpectedDecls
top::Decls ::= wrn::String decls::Decls
{
  decls.moduleName = top.moduleName;

  decls.tyEnv = top.tyEnv;
  decls.constructorEnv = top.constructorEnv;
  decls.judgmentEnv = top.judgmentEnv;
  decls.translationEnv = top.translationEnv;
  decls.ruleEnv = top.ruleEnv;

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

  forwards to decls;
}

