grammar sos:core;


imports sos:core:semanticDefs:concreteSyntax;
exports sos:core:semanticDefs:abstractSyntax;

imports sos:core:modules;

imports sos:core:concreteDefs:concreteSyntax;

imports sos:core:main:concreteSyntax;


import silver:util:cmdargs;


parser abstractSyntaxParser::File_c {
  sos:core:common:concreteSyntax;
  sos:core:semanticDefs:concreteSyntax;
}

parser concreteSyntaxParser::ConcreteFile_c {
  sos:core:common:concreteSyntax;
  sos:core:concreteDefs:concreteSyntax;
}

parser mainFileParser::MainFile_c {
  sos:core:common:concreteSyntax;
  sos:core:semanticDefs:concreteSyntax;
  sos:core:main:concreteSyntax;
}


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, abstractSyntaxParser, concreteSyntaxParser,
             mainFileParser, ioin);
}


function run
IOVal<Integer> ::= args::[String]
   abstractFileParse::(ParseResult<File_c> ::= String String)
   concreteFileParse::(ParseResult<ConcreteFile_c> ::= String String)
   mainFileParse::(ParseResult<MainFile_c> ::= String String)
   ioin::IOToken
{
  --(result ::= compiled mods  gen loc  grammars loc  args  io)
  production attribute actions::[ActionSpec] with ++;
  actions :=
     [
      actionSpec(
         runFun =
            \ m::ModuleList gen::String grmmrs::String
              a::Decorated CmdArgs i::IOToken ->
              let message::IOToken =
                  printT("Checking for errors\n", i)
              in
                if m.errorString != ""
                then ioval(printT(m.errorString ++ "\n", message), 1)
                else ioval(printT("No errors found\n", message), 0)
              end,
         shouldDoFun = \ a::Decorated CmdArgs -> true,
         actionDesc = "Error Checking")
     ];

  local e::Either<String  Decorated CmdArgs> = parseArgs(args);
  local a::Decorated CmdArgs = e.fromRight;
  local rootLoc::String =
        if null(a.rootLoc) then "" else head(a.rootLoc);

  local modules::IOVal<Either<String ModuleList>> =
        buildModuleList(a.generateModuleName, rootLoc,
           abstractFileParse, concreteFileParse, mainFileParse, ioin);
  local genLoc::IOVal<String> =
        envVarT("SOS_GENERATED", modules.io);
  local grmmrsLoc::IOVal<String> =
        envVarT("SOS_GRAMMARS", genLoc.io);

  local startMessage::IOToken =
        printT("\n*************** SOS-Ext ***************\n",
               grmmrsLoc.io);
  local runs::IOVal<Integer> =
        runActions(actions, modules.iovalue.fromRight, genLoc.iovalue,
                   grmmrsLoc.iovalue, a, startMessage);

  return
     case e of
     | left(err) ->
       ioval(printT("Error parsing commandline input:\n" ++ err,
                    ioin), 1)
     | right(_) ->
       case modules.iovalue of
       | left(err) ->
         ioval(printT(err ++ "\n", modules.io), 1)
       | right(_) ->
         ioval(printT("\n", runs.io), runs.iovalue)
       end
     end;
}


--run all the actions in the order in which they occur
function runActions
IOVal<Integer> ::=
    actions::[ActionSpec] mods::ModuleList genLoc::String
    grmmrsLoc::String a::Decorated CmdArgs ioin::IOToken
{
  local act::ActionSpec = head(actions);
  local pre::IOToken =
      printT("\n---------------------------------------\n\n", ioin);
  local runAct::IOVal<Integer> =
      act.runFun(mods, genLoc, grmmrsLoc, a, pre);

  return
      case actions of
      | [] -> ioval(ioin, 0)
      | _::tl when act.shouldDoFun(a) ->
        if runAct.iovalue != 0 --error in this action
        then runAct
        else runActions(tl, mods, genLoc, grmmrsLoc, a, runAct.io)
      | _::tl -> runActions(tl, mods, genLoc, grmmrsLoc, a, ioin)
      end;
}


--location and grammar name of final, runnable Silver grammar
function buildFinalGrammar
(String, String) ::= module::String genLoc::String
{
  return (genLoc ++ (if endsWith("/", genLoc) then "" else "/") ++
             "main/" ++ implode("/", explode(":", module)),
          "main:" ++ module);
}






nonterminal ActionSpec with runFun, shouldDoFun, actionDesc;
--How to run the action (return 0 for success, non-zero for fail)
--Any printed output should end with a single newline
--   result ::= compiled mods  gen loc  grammars loc  args  io
annotation runFun::(IOVal<Integer> ::= ModuleList  String  String
                                       Decorated CmdArgs  IOToken);
--Whether to run the action (true => run, false => skip)
--Should not do any IO actions
annotation shouldDoFun::(Boolean ::= Decorated CmdArgs);
--A short name/description of the action, currently for debugging
--May be made part of the standard output in the future
annotation actionDesc::String;

production actionSpec
top::ActionSpec ::=
{ }





attribute errors, generateModuleName, rootLoc occurs on CmdArgs;

synthesized attribute errors::[String];
synthesized attribute generateModuleName::String;
synthesized attribute rootLoc::[String];

aspect production endCmdArgs
top::CmdArgs ::= l::[String]
{
  top.errors =
      case l of
      | [] -> ["Must give a module name"]
      | [_] -> []
      | l -> ["Cannot give more than one module name; found " ++
              toString(length(l))]
      end;

  top.generateModuleName = head(l);

  top.rootLoc = [];
}


abstract production locationOption
top::CmdArgs ::= loc::String rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  top.rootLoc = loc::rest.rootLoc;

  forwards to rest;
}



function parseArgs
Either<String  Decorated CmdArgs> ::= args::[String]
{
  production attribute flags::[FlagSpec] with ++;
  flags := [];

  flags <-
     [flagSpec(name="-I",
               paramString=just("<path>"),
               help="path to modules",
               flagParser=option(locationOption))];

  local usage::String = 
        "Usage: sos-ext [options] <module name>\n\n" ++
        "Flag options:\n" ++ flagSpecsToHelpText(flags) ++ "\n";

  -- Parse the command line
  production a::CmdArgs = interpretCmdArgs(flags, args);

  production attribute errors::[String] with ++;
  errors := a.errors;
  errors <-
     if length(a.rootLoc) > 1
     then ["Can only give one location; found " ++
           toString(length(a.rootLoc))]
     else [];

  return if !null(a.errors)
         then left(implode("\n", errors) ++ "\n\n" ++ usage)
         else right(a);
}

