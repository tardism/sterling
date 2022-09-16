grammar sos:core:concreteDefs:concreteSyntax;


closed nonterminal ConcreteFile_c;

concrete productions top::ConcreteFile_c
| name::ModuleDecl_c x::EmptyNewlines d::TopDeclList_c
{ }




closed nonterminal TopDecl_c;
closed nonterminal TopDeclList_c;

concrete productions top::TopDecl_c
| d::TerminalDecl_c
{ }
| d::ConcreteSyntaxDecl_c
{ }

concrete productions top::TopDeclList_c
|
{ }
| d::TopDecl_c x::EmptyNewlines rest::TopDeclList_c
{ }




closed nonterminal TerminalDecl_c;

concrete productions top::TerminalDecl_c
| tmnl::LowerId_t r::Regex_c
{ }
| 'ignore' r::Regex_c
{ }




closed nonterminal ConcreteSyntaxDecl_c;
closed nonterminal ConcreteProdDecls_c;
closed nonterminal ConcreteProdDecl_c;

concrete productions top::ConcreteSyntaxDecl_c
| ntmnl::LowerId_t '<' ty::Type_c '>' '::=' d::ConcreteProdDecls_c
{ }
| ntmnl::LowerId_t '<' ty::Type_c '>' '::=' '|' d::ConcreteProdDecls_c
{ }
| ntmnl::LowerId_t '<' ty::Type_c '>' '::=' '.' '.' '.' x::EmptyNewlines d::ConcreteProdDecls_c
{ }
| ntmnl::LowerQName_t '<' ty::Type_c '>' '::=' '.' '.' '.' x::EmptyNewlines '|' d::ConcreteProdDecls_c
{ }

concrete productions top::ConcreteProdDecls_c
| d::ConcreteProdDecl_c Newline_t
{ }
| d::ConcreteProdDecl_c '|' rest::ConcreteProdDecls_c
{ }
| d::ConcreteProdDecl_c Newline_t '|' rest::ConcreteProdDecls_c
{ }
| Newline_t rest::ConcreteProdDecls_c
{ }

concrete productions top::ConcreteProdDecl_c
| p::ProductionElements_c '~~>' t::Term_c
{ }




closed nonterminal ProductionElement_c;
closed nonterminal ProductionElements_c;

concrete productions top::ProductionElement_c
| name::LowerId_t
{ }
| name::LowerQName_t
{ }

concrete productions top::ProductionElements_c
|
{ }
| e::ProductionElement_c rest::ProductionElements_c
{ }




closed nonterminal Regex_c;
closed nonterminal RegexGroup_c;

concrete productions top::Regex_c
| r::Regex_c '*'
{ }
| r::Regex_c '+'
{ }
| '(' r::Regex_c ')'
{ }
| c::Char_t
{ }
| '[' g::RegexGroup_c ']'
{ }

concrete productions top::RegexGroup_c
|
{ }
| c::Char_t rest::RegexGroup_c
{ }
| c1::Char_t '-' c2::Char_t rest::RegexGroup_c
{ }




closed nonterminal Term_c;
closed nonterminal TermList_c;

concrete productions top::Term_c
| constant::LowerId_t
{ }
| constant::LowerQName_t
{ }
| constant::LowerId_t '(' x::EmptyNewlines ')'
{ }
| constant::LowerQName_t '(' x::EmptyNewlines ')'
{ }
| prod::LowerId_t '(' x1::EmptyNewlines args::TermList_c x2::EmptyNewlines ')'
{ }
| prod::LowerQName_t '(' x1::EmptyNewlines args::TermList_c x2::EmptyNewlines ')'
{ }
| index::ProdPart_t --index into the nonterminals in the production
{ }
| '$to_int' '(' x1::EmptyNewlines t::Term_c x2::EmptyNewlines ')'
{ }
| t::Term_c '[' x1::EmptyNewlines i::Integer_t x2::EmptyNewlines ':' x3::EmptyNewlines j::Integer_t x4::EmptyNewlines ']'
{ }
| t::Term_c '[' x1::EmptyNewlines i::Integer_t x2::EmptyNewlines ':' x3::EmptyNewlines ']'
{ }
| t::Term_c '[' x1::EmptyNewlines ':' x2::EmptyNewlines j::Integer_t x3::EmptyNewlines ']'
{ }
| i::Integer_t
{ }
| s::String_t
{ }

concrete productions top::TermList_c
| t::Term_c
{ }
| t::Term_c ',' x::EmptyNewlines rest::TermList_c
{ }

