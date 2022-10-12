grammar sos:core:main:concreteSyntax;


closed nonterminal MainFile_c layout {Spacing_t, Comment_t}
   with ast<MainFile>, location;

concrete productions top::MainFile_c
| name::ModuleDecl_c x::EmptyNewlines d::MainDeclList_c
  { top.ast = mainFile(name.ast, d.ast, location=top.location); }



closed nonterminal MainDeclList_c layout {Spacing_t, Comment_t}
   with ast<MainDecls>, location;

concrete productions top::MainDeclList_c
|
  { top.ast = emptyMainDecls(location=top.location); }
| d::MainDecl_c x::EmptyNewlines rest::MainDeclList_c
  { top.ast = branchMainDecls(d.ast, rest.ast, location=top.location); }



closed nonterminal MainDecl_c layout {Spacing_t, Comment_t}
   with ast<MainDecls>, location;

concrete productions top::MainDecl_c
| p::ParseDecl_c
  { top.ast = parseMainDecl(p.ast, location=top.location); }
| r::DeriveRelation_c
  { top.ast = deriveMainDecl(r.ast, location=top.location); }
| name::UpperId_t ':=' e::MainExpr_c Newline_t
  { }
| 'While' x1::EmptyNewlines e::MainExpr_c x2::EmptyNewlines 'Do'
  x3::EmptyNewlines body::MainDeclList_c x4::EmptyNewlines 'End'
  { }
| 'If' x1::EmptyNewlines e::MainExpr_c x2::EmptyNewlines
  'Then' x3::EmptyNewlines th::MainDeclList_c x4::EmptyNewlines
  'Else' x5::EmptyNewlines el::MainDeclList_c x6::EmptyNewlines 'End'
  { }



closed nonterminal ParseDecl_c layout {Spacing_t, Comment_t}
   with ast<ParseDecl>, location;

concrete productions top::ParseDecl_c
| 'Parse' x1::EmptyNewlines nt::LowerQName_t x2::EmptyNewlines 'as'
  x3::EmptyNewlines var::UpperId_t x3::EmptyNewlines
  'from' x4::EmptyNewlines e::MainExpr_c
  { top.ast = parseDecl(toQName(nt, location=nt.location), var.lexeme,
                        e.ast, location=top.location); }



closed nonterminal DeriveRelation_c layout {Spacing_t, Comment_t}
   with ast<Derive>, location;

concrete productions top::DeriveRelation_c
| j::Judgment_c
{ top.ast = deriveRelation(j.ast, location=top.location); }



closed nonterminal MainExpr_c layout {Spacing_t, Comment_t};
closed nonterminal AndExpr_c layout {Spacing_t, Comment_t};
closed nonterminal CompExpr_c layout {Spacing_t, Comment_t};
closed nonterminal AddExpr_c layout {Spacing_t, Comment_t};
closed nonterminal MultExpr_c layout {Spacing_t, Comment_t};
closed nonterminal FactorExpr_c layout {Spacing_t, Comment_t};

concrete productions top::MainExpr_c
| e1::MainExpr_c '||' e2::AndExpr_c
{ }
| e::AndExpr_c
{ }

concrete productions top::AndExpr_c
| e1::AndExpr_c '&&' e2::CompExpr_c
{ }
| e::CompExpr_c
{ }

concrete productions top::CompExpr_c
| e1::CompExpr_c '<' e2::AddExpr_c
{ }
| e1::CompExpr_c '>' e2::AddExpr_c
{ }
| e1::CompExpr_c '<=' e2::AddExpr_c
{ }
| e1::CompExpr_c '>=' e2::AddExpr_c
{ }
| e::AddExpr_c
{ }

concrete productions top::AddExpr_c
| e1::AddExpr_c '+' e2::MultExpr_c
{ }
| e1::AddExpr_c '-' e2::MultExpr_c
{ }
| e::MultExpr_c
{ }

concrete productions top::MultExpr_c
| e1::MultExpr_c '*' e2::FactorExpr_c
{ }
| e1::MultExpr_c '/' e2::FactorExpr_c
{ }
| e1::MultExpr_c '%' e2::FactorExpr_c
{ }
| e::FactorExpr_c
{ }

concrete productions top::FactorExpr_c
| name::VarId_t
{ }
| num::Int_t
{ }
| '(' e::MainExpr_c ')'
{ }
