grammar sos:core;


imports sos:core:semanticDefs:concreteSyntax;
exports sos:core:semanticDefs:abstractSyntax;

imports sos:core:modules;

imports sos:core:concreteDefs:concreteSyntax;


import silver:util:cmdargs;


parser abstractSyntaxParser::File_c {
  sos:core:common:concreteSyntax;
  sos:core:semanticDefs:concreteSyntax;
}

parser concreteSyntaxParser::ConcreteFile_c {
  sos:core:common:concreteSyntax;
  sos:core:concreteDefs:concreteSyntax;
}


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, abstractSyntaxParser, concreteSyntaxParser, ioin);
}


function run
IOVal<Integer> ::= args::[String]
   abstractFileParse::(ParseResult<File_c> ::= String String)
   concreteFileParse::(ParseResult<ConcreteFile_c> ::= String String)
   ioin::IOToken
{
  --String is location of generated directory
  production attribute actions::[(IOVal<Integer> ::= ModuleList String
                                  Decorated CmdArgs IOToken)] with ++;
  actions :=
     [
      \ m::ModuleList gen::String a::Decorated CmdArgs i::IOToken ->
        if m.errorString != ""
        then ioval(printT(m.errorString ++ "\n", i), 1)
        else ioval(printT("No errors found\n", i), 0)
     ];

  local e::Either<String Decorated CmdArgs> = parseArgs(args);
  local a::Decorated CmdArgs = e.fromRight;
  local rootLoc::String =
        if null(a.rootLoc) then "" else head(a.rootLoc);

  local modules::IOVal<Either<String ModuleList>> =
        buildModuleList(a.generateModuleName, rootLoc,
                        abstractFileParse, concreteFileParse, ioin);
  local genLoc::IOVal<String> =
        envVarT("SOS_GENERATED", modules.io);

  return
     case e of
     | left(err) ->
       ioval(printT("Error parsing commandline input:\n" ++ err,
                    ioin), 1)
     | right(_) ->
       case modules.iovalue of
       | left(err) ->
         ioval(printT(err ++ "\n", modules.io), 1)
       | right(mods) ->
         --run all actions in the order in which they occur
         foldl(\ rest::IOVal<Integer>
                 act::(IOVal<Integer> ::= ModuleList String
                          Decorated CmdArgs IOToken) ->
                 if rest.iovalue != 0 --error in a previous action
                 then rest
                 else act(mods, genLoc.iovalue, a, rest.io),
               ioval(genLoc.io, 0), actions)
       end
     end;
}




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
        "Usage: <this program> [options] <module name>\n\n" ++
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

