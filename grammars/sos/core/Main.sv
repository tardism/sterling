grammar sos:core;


imports sos:core:concreteSyntax;
imports sos:core:abstractSyntax;


parser p::File_c {
  sos:core:concreteSyntax;
}


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  local filename::String = head(args);
  return run(filename, p, ioin);
}


function run
IOVal<Integer> ::= filename::String
                   p::(ParseResult<File_c> ::= String String)
                   ioin::IOToken
{
  local contents::IOVal<String> = readFileT(filename, ioin);
  local parsed::ParseResult<File_c> = p(contents.iovalue, filename);
  local file::File = parsed.parseTree.ast;
  return if !parsed.parseSuccess
         then ioval(printT("Parse failed:\n" ++ parsed.parseErrors ++
                           "\n", contents.io), 1)
         else if length(file.errors) > 0
         then ioval(printT("Errors:\n   " ++
                           implode("\n   ", map((.pp),
                                   file.errors)) ++ "\n",
                           contents.io), 1)
         else ioval(printT("Parse success:\n" ++ file.pp ++ "\n",
                           contents.io), 0);
}

