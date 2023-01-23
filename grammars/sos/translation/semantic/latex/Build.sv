grammar sos:translation:semantic:latex;


import silver:util:cmdargs;
import sos:core:modules;
import sos:core;


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, abstractSyntaxParser, concreteSyntaxParser,
             mainFileParser, ioin);
}


aspect function run
IOVal<Integer> ::= _ _ _ _ _
{
  actions <- [runLaTeX];
}


function runLaTeX
IOVal<Integer> ::= m::ModuleList genLoc::String grmmrsLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local latexString::String = m.ppLaTeX;

  local message::IOToken = printT("Producing LaTeX output\n", i);

  local fileLoc::String = head(a.latexLocation);
  local output::IOToken = writeFileT(fileLoc, latexString, message);

  return if null(a.latexLocation) then ioval(i, 0)
                                  else ioval(output, 0);
}




synthesized attribute latexLocation::[String] occurs on CmdArgs;

aspect production endCmdArgs
top::CmdArgs ::= l::[String]
{
  top.latexLocation = [];
}


abstract production latexOption
top::CmdArgs ::= filename::String rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  top.rootLoc = rest.rootLoc;

  top.latexLocation = filename::rest.latexLocation;

  forwards to rest;
}


aspect function parseArgs
Either<String  Decorated CmdArgs> ::= args::[String]
{
  flags <-
     [flagSpec(name="--latex",
               paramString=just("<filename>"),
               help="filename for LaTeX output",
               flagParser=option(latexOption))];

  errors <-
     if length(a.latexLocation) > 1
     then ["Can only give one location for LaTeX output; found " ++
           toString(length(a.latexLocation))]
     else [];
}
