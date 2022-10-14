grammar sos:translation:semantic:prolog;


import silver:util:cmdargs;
import sos:core:modules;
import sos:core;


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, abstractSyntaxParser, concreteSyntaxParser, ioin);
}


aspect function run
IOVal<Integer> ::= _ _ _ _
{
  actions <- [runProlog];
}


function runProlog
IOVal<Integer> ::= m::ModuleList a::Decorated CmdArgs i::IOToken
{
  local prologProgram::PrologProgram =
        buildPrologProgram(m.prologRules);
  local prologString::String = prologProgram.pp;

  local message::IOToken = printT("Producing Prolog output\n", i);

  local fileLoc::String = head(a.prologLocation);
  local output::IOToken = writeFileT(fileLoc, prologString, message);

  return if null(a.prologLocation) then ioval(i, 0)
                                   else ioval(output, 0);
}




synthesized attribute prologLocation::[String] occurs on CmdArgs;

aspect production endCmdArgs
top::CmdArgs ::= l::[String]
{
  top.prologLocation = [];
}


abstract production prologOption
top::CmdArgs ::= filename::String rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  top.rootLoc = rest.rootLoc;

  top.prologLocation = filename::rest.prologLocation;

  forwards to rest;
}


aspect function parseArgs
Either<String  Decorated CmdArgs> ::= args::[String]
{
  flags <-
     [flagSpec(name="--prolog",
               paramString=just("<filename>"),
               help="filename for Prolog output",
               flagParser=option(prologOption))];

  errors <-
     if length(a.prologLocation) > 1
     then ["Can only give one location for Prolog output; fonud " ++
           toString(length(a.prologLocation))]
     else [];
}

