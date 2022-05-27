grammar sos:composed;


imports sos:core;

imports sos:testing;
imports sos:translation:prolog;



function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, p, ioin);
}

