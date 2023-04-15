grammar sos:core:concreteDefs:concreteSyntax;


closed nonterminal ConcreteFile_c layout {Spacing_t, Comment_t}
   with ast<ConcreteFile>, location;

concrete productions top::ConcreteFile_c
| name::ModuleDecl_c x::EmptyNewlines d::TopDeclList_c
  { top.ast = concreteFile(name.ast, d.ast, location=top.location); }




closed nonterminal TopDecl_c layout {Spacing_t, Comment_t}
   with ast<ConcreteDecls>, location;
closed nonterminal TopDeclList_c layout {Spacing_t, Comment_t}
   with ast<ConcreteDecls>, location;

concrete productions top::TopDecl_c
| d::TerminalDecl_c
  { top.ast = terminalDecl(d.ast, location=top.location); }
| d::ConcreteSyntaxDecl_c
  { top.ast = concreteSyntaxDecl(d.ast, location=top.location); }

concrete productions top::TopDeclList_c
|
  { top.ast = nilDecls(location=top.location); }
| d::TopDecl_c x::EmptyNewlines rest::TopDeclList_c
  { top.ast = branchConcreteDecls(d.ast, rest.ast,
                                  location=top.location); }




closed nonterminal TerminalDecl_c layout {Spacing_t, Comment_t}
   with ast<TerminalDecl>, location;

concrete productions top::TerminalDecl_c
| tmnl::LowerId_t '/' r::Regex_c '/'
  { top.ast = useTerminal(tmnl.lexeme, r.ast,
                          location=top.location); }
| 'ignore' '/' r::Regex_c '/'
  { top.ast = ignoreTerminal(r.ast, location=top.location); }




closed nonterminal ConcreteSyntaxDecl_c layout {Spacing_t, Comment_t}
   with ast<ConcreteSyntaxDecl>, location;
closed nonterminal ConcreteProdDecls_c layout {Spacing_t, Comment_t}
   with ast<ConcreteProdDecls>, location;
closed nonterminal ConcreteProdDecl_c layout {Spacing_t, Comment_t}
   with ast<ConcreteProdDecls>, location;

concrete productions top::ConcreteSyntaxDecl_c
| ntmnl::LowerId_t '<' ty::Type_c '>' '::=' d::ConcreteProdDecls_c
  { top.ast = newConcreteNonterminal(ntmnl.lexeme, ty.ast, d.ast,
                 location=top.location); }
| ntmnl::LowerId_t '<' ty::Type_c '>' '::=' x::EmptyNewlines
  '|' d::ConcreteProdDecls_c
  { top.ast = newConcreteNonterminal(ntmnl.lexeme, ty.ast, d.ast,
                 location=top.location); }
| ntmnl::LowerId_t '::=' '.' '.' '.' x::EmptyNewlines
  '|' d::ConcreteProdDecls_c
  { top.ast = addConcreteNonterminal(
                 toQName(ntmnl.lexeme, ntmnl.location),
                 d.ast, location=top.location); }
| ntmnl::LowerQName_t '::=' '.' '.' '.' x::EmptyNewlines
  '|' d::ConcreteProdDecls_c
  { top.ast = addConcreteNonterminal(
                 toQName(ntmnl.lexeme, ntmnl.location),
                 d.ast, location=top.location); }

concrete productions top::ConcreteProdDecls_c
| d::ConcreteProdDecl_c Newline_t
  { top.ast = d.ast; }
| d::ConcreteProdDecl_c '|' rest::ConcreteProdDecls_c
  { top.ast = branchConcreteProdDecls(d.ast, rest.ast,
                                      location=top.location); }
| d::ConcreteProdDecl_c Newline_t '|' rest::ConcreteProdDecls_c
  { top.ast = branchConcreteProdDecls(d.ast, rest.ast,
                                      location=top.location); }
| Newline_t rest::ConcreteProdDecls_c
  { top.ast = rest.ast; }

concrete productions top::ConcreteProdDecl_c
| p::ProductionElements_c '~~>' x2::EmptyNewlines
  t::Term_c
  { top.ast = concreteProdDecl(p.ast, t.ast, location=top.location); }




closed nonterminal ProductionElement_c layout {Spacing_t, Comment_t}
   with ast<ProductionElement>, location;
closed nonterminal ProductionElements_c layout {Spacing_t, Comment_t}
   with ast<ProductionElement>, location;

concrete productions top::ProductionElement_c
| var::ProdPart_t '::' name::LowerId_t
  { top.ast = nameProductionElement(var.lexeme,
                 toQName(name.lexeme, name.location),
                 location=top.location); }
| var::ProdPart_t '::' name::LowerQName_t
  { top.ast = nameProductionElement(var.lexeme,
                 toQName(name.lexeme, name.location),
                 location=top.location); }
| name::LowerId_t
  { top.ast = unnamedProductionElement(
                 toQName(name.lexeme, name.location),
                 location=top.location); }
| name::LowerQName_t
  { top.ast = unnamedProductionElement(
                 toQName(name.lexeme, name.location),
                 location=top.location); }

concrete productions top::ProductionElements_c
|
  { top.ast = emptyProductionElement(location=top.location); }
| e::ProductionElement_c x::EmptyNewlines rest::ProductionElements_c
  { top.ast = branchProductionElement(e.ast, rest.ast,
                                      location=top.location); }




closed nonterminal Regex_c layout {Spacing_t, Comment_t}
   with ast<Regex>, location;
closed nonterminal RegexStep_c layout {Spacing_t, Comment_t}
   with ast<Regex>, location;
closed nonterminal SingleRegex_c layout {Spacing_t, Comment_t}
   with ast<Regex>, location;
closed nonterminal RegexGroup_c layout {Spacing_t, Comment_t}
   with ast<RegexGroup>, location;

concrete productions top::Regex_c
| r::RegexStep_c
  { top.ast = r.ast; }
| r::SingleRegex_c rest::Regex_c
  { top.ast = concatRegex(r.ast, rest.ast, location=r.location); }

concrete productions top::RegexStep_c
| r::SingleRegex_c
  { top.ast = r.ast; }
| r1::RegexStep_c '|' r2::SingleRegex_c
  { top.ast = alternateRegex(r1.ast, r2.ast, location=top.location); }

concrete productions top::SingleRegex_c
| r::SingleRegex_c '*'
  { top.ast = starRegex(r.ast, location=top.location); }
| r::SingleRegex_c '+'
  { top.ast = plusRegex(r.ast, location=top.location); }
| '(' r::Regex_c ')'
  { top.ast = r.ast; }
| c::Char_t
  { top.ast = charRegex(c.lexeme, location=top.location); }
| '-' --to deal with the precedence between Char_t and '-'
  { top.ast = charRegex("-", location=top.location); }
| '[' g::RegexGroup_c ']'
  { top.ast = groupRegex(g.ast, location=top.location); }

concrete productions top::RegexGroup_c
|
  { top.ast = emptyRegexGroup(location=top.location); }
| c::Char_t rest::RegexGroup_c
  { top.ast = branchRegexGroup(
                 charRegexGroup(c.lexeme, location=c.location),
                 rest.ast, location=top.location); }
| c1::Char_t '-' c2::Char_t rest::RegexGroup_c
  { top.ast = branchRegexGroup(
                 rangeRegexGroup(c1.lexeme, c2.lexeme,
                                 location=c1.location),
                 rest.ast, location=top.location); }




closed nonterminal Term_c layout {Spacing_t, Comment_t}
   with ast<Term>, location;
closed nonterminal Term2_c layout {Spacing_t, Comment_t}
   with ast<Term>, location;
closed nonterminal Term3_c layout {Spacing_t, Comment_t}
   with ast<Term>, location;
closed nonterminal TermList_c layout {Spacing_t, Comment_t}
   with ast<TermList>, listAST, location;

synthesized attribute listAST::Term;

concrete productions top::Term_c
| hd::Term2_c '::' tl::Term_c
  { top.ast = consTerm(hd.ast, tl.ast, location=top.location); }
| t::Term2_c
  { top.ast = t.ast; }

concrete productions top::Term2_c
| t::Term2_c '[' x1::EmptyNewlines i::Integer_t x2::EmptyNewlines ':'
  x3::EmptyNewlines j::Integer_t x4::EmptyNewlines ']'
  { top.ast = substringTerm(t.ast, just(toInteger(i.lexeme)),
                 just(toInteger(j.lexeme)), location=top.location); }
| t::Term2_c '[' x1::EmptyNewlines i::Integer_t x2::EmptyNewlines ':'
  x3::EmptyNewlines ']'
  { top.ast = substringTerm(t.ast, just(toInteger(i.lexeme)),
                 nothing(), location=top.location); }
| t::Term2_c '[' x1::EmptyNewlines ':' x2::EmptyNewlines j::Integer_t
  x3::EmptyNewlines ']'
  { top.ast = substringTerm(t.ast, nothing(), just(toInteger(j.lexeme)),
                 location=top.location); }
| t::Term3_c
  { top.ast = t.ast; }

concrete productions top::Term3_c
| constant::LowerId_t
  { top.ast = nameTerm(toQName(constant.lexeme, constant.location),
                       location=top.location); }
| constant::LowerQName_t
  { top.ast = nameTerm(toQName(constant.lexeme, constant.location),
                       location=top.location); }
| constant::LowerIdParen_t --'(' is part of this
  x::EmptyNewlines ')'
  { top.ast = nameTerm(toQName(dropNameParen(constant.lexeme),
                               constant.location),
                       location=top.location); }
| constant::LowerQNameParen_t --'(' is part of this
  x::EmptyNewlines ')'
  { top.ast = nameTerm(toQName(dropNameParen(constant.lexeme),
                               constant.location),
                       location=top.location); }
| prod::LowerIdParen_t --'(' is part of this
  x1::EmptyNewlines args::TermList_c x2::EmptyNewlines ')'
  { top.ast = applicationTerm(toQName(dropNameParen(prod.lexeme),
                                      prod.location),
                              args.ast, location=top.location); }
| prod::LowerQNameParen_t --'(' is part of this
  x1::EmptyNewlines args::TermList_c x2::EmptyNewlines ')'
  { top.ast = applicationTerm(toQName(dropNameParen(prod.lexeme),
                                      prod.location),
                              args.ast, location=top.location); }
| index::ProdPart_t
  { top.ast = prodIndex(index.lexeme, location=top.location); }
| '$to_int' '(' x1::EmptyNewlines t::Term_c x2::EmptyNewlines ')'
  { top.ast = toIntTerm(t.ast, location=top.location); }
| i::Integer_t
  { top.ast = intTerm(toInteger(i.lexeme), location=top.location); }
| s::String_t
  { top.ast = stringTerm(substring(1, length(s.lexeme) - 1, s.lexeme),
                    location=top.location); }
| '[' ']'
  { top.ast = nilTerm(location=top.location); }
| '[' tms::TermList_c ']'
  { top.ast = tms.listAST; }
| '(' tms::TermList_c ')'
  { top.ast = tupleTerm(tms.ast, location=top.location); }

concrete productions top::TermList_c
| t::Term_c
  { top.ast = singleTermList(t.ast, location=top.location);
    top.listAST = consTerm(t.ast, nilTerm(location=top.location),
                           location=top.location); }
| t::Term_c ',' x::EmptyNewlines rest::TermList_c
  { top.ast = branchTermList(
                 singleTermList(t.ast, location=t.location),
                 rest.ast, location=top.location);
    top.listAST = consTerm(t.ast, rest.listAST,
                           location=top.location); }

