grammar sos:translation:semantic:prolog:parseProlog;

import sos:core:common:abstractSyntax;
import sos:core:semanticDefs:abstractSyntax;


nonterminal PrologOutput with result;

synthesized attribute result::Maybe<[(String, Term)]>;

concrete productions top::PrologOutput
| l::OutputList
  { top.result = just(l.bindings); }
| l::OutputList '.'
  { top.result = just(l.bindings); }
| 'false' '.' e::Error_t
  { top.result = nothing(); }



nonterminal OutputList with bindings;

synthesized attribute bindings::[(String, Term)];

concrete productions top::OutputList
| var::VarName_t '=' t::PrologTerm
  { top.bindings = [(var.lexeme, t.tm)]; }
| var::VarName_t '=' t::PrologTerm ',' rest::OutputList
  { top.bindings = (var.lexeme, t.tm)::rest.bindings; }





nonterminal PrologTerm with tm;
nonterminal PrologTermList with tmList;

synthesized attribute tm::Term;
synthesized attribute tmList::TermList;

concrete productions top::PrologTerm
| name::Constr_t
  { top.tm = const(constrQName(name.lexeme), location=bogusLoc()); }
| name::Constr_t '(' ')'
  { top.tm = const(constrQName(name.lexeme), location=bogusLoc()); }
| name::Constr_t '(' args::PrologTermList ')'
  { top.tm = appTerm(constrQName(name.lexeme), args.tmList,
                     location=bogusLoc()); }
| str::String_t
  { top.tm =
        stringConst(substring(1, length(str.lexeme) - 1, str.lexeme),
                    location=bogusLoc()); }
| i::Int_t
  { top.tm = num(toInteger(i.lexeme), location=bogusLoc()); }
| '(' t1::PrologTerm ',' t2::PrologTerm ')'
  { top.tm = tupleTerm(consTermList(t1.tm, 
                        consTermList(t2.tm, 
                                      nilTermList(location=bogusLoc()),
                                      location=bogusLoc()),
                       location=bogusLoc()),
                       location=bogusLoc()); }
| '[' ']'
  { top.tm = nilTerm(location=bogusLoc()); }
-- can use nilTerm because sterling is not supporting return a list in .main, so it's kind of a placeholder
| '[' args::PrologTermList ']'
  { top.tm = nilTerm(location=bogusLoc());}

concrete productions top::PrologTermList
| t::PrologTerm
  { top.tmList = consTermList(t.tm, nilTermList(location=bogusLoc()),
                              location=bogusLoc()); }
| t::PrologTerm ',' rest::PrologTermList
  { top.tmList =
        consTermList(t.tm, rest.tmList, location=bogusLoc()); }





function constrQName
QName ::= name::String
{
  local removeFront::String = substring(9, length(name), name);
  local split::[String] = explode("_MODULE_", removeFront);
  return foldrLastElem(moduleLayerName(_, _, location=bogusLoc()),
                       baseName(_, location=bogusLoc()), split);
}


-- the function is not working yet, it is not used
-- function termListToConsTerm
-- Term ::= tl::TermList
-- {
--   return
--     case tl of
--     | nilTermList() -> nilTerm(location=bogusLoc())
--     | consTermList(hd, rest) -> 
--       consTerm(hd, termListToConsTerm(rest), location=bogusLoc())
--     end;
-- }



terminal False_t    'false';
terminal Dot_t      '.';
terminal Eq_t       '=';
terminal Comma_t    ',';
terminal LParen_t   '(';
terminal RParen_t   ')';
terminal LBracket_t '[';
terminal RBracket_t ']';

terminal VarName_t   /[A-Z][a-zA-Z0-9_]*/;
terminal Constr_t    /[a-z][a-zA-Z0-9_]*/;

terminal String_t    /"([^"]|(\\"))*"/;
terminal Int_t       /[0-9]+/;

terminal Error_t   /Error: Stream.+/;

ignore terminal   Whitespace_t   /[\n\t\ \r]+/;