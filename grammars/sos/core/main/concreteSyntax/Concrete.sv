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
| d::FunDecl_c x::EmptyNewlines rest::MainDeclList_c
  { top.ast = branchMainDecls(d.ast, rest.ast, location=top.location); }



closed nonterminal FunDecl_c layout {Spacing_t, Comment_t}
   with ast<FunDecl>, location;
closed nonterminal Params_c layout {Spacing_t Comment_t}
   with ast<Params>, location;

concrete productions top::FunDecl_c
-- Function name : params -> return type { body }
| 'Function' x1::EmptyNewlines name::LowerId_t x2::EmptyNewlines ':'
  x3::EmptyNewlines p::Params_c x4::EmptyNewlines '->'
  x5::EmptyNewlines ty::Type_c x6::EmptyNewlines '{'
  x7::EmptyNewlines body::StmtList_c x8::EmptyNewlines '}'
{ }

concrete productions top::Params_c
|
{ }
| '<' name::UpperId_t ':' ty::Type_c '>' rest::Params_c
{ }



--Add to existing types
concrete productions top::Type_c
| '[' ty::Type_c ']'
{ }
| '?result'
{ }



closed nonterminal StmtList_c layout {Spacing_t, Comment_t}
   with ast<Stmt>, location;
closed nonterminal Stmt_c layout {Spacing_t, Comment_c}
   with ast<Stmt>, location;

concrete productions top::StmtList_c
|
  { top.ast = noop(location=top.location); }
| s::Stmt_c Newline_t rest::StmtList_c
  { top.ast = branchStmt(s.ast, rest.ast, location=top.location); }

concrete productions top::Stmt_c
| p::Parse_c
  { top.ast = parseStmt(p.ast, location=top.location); }
| r::DeriveRelation_c
  { top.ast = deriveRelStmt(r.ast, location=top.location); }
| name::UpperId_t ':=' e::MainExpr_c Newline_t
  { top.ast = assignStmt(name.lexeme, e.ast, location=top.location); }
| 'While' x1::EmptyNewlines e::MainExpr_c x2::EmptyNewlines 'Do'
  x3::EmptyNewlines body::StmtList_c x4::EmptyNewlines 'End'
  { top.ast = whileStmt(e.ast, body.ast, location=top.location); }
| 'If' x1::EmptyNewlines e::MainExpr_c x2::EmptyNewlines
  'Then' x3::EmptyNewlines th::StmtList_c x4::EmptyNewlines
  'Else' x5::EmptyNewlines el::StmtList_c x6::EmptyNewlines 'End'
  { top.ast = ifStmt(e.ast, th.ast, el.ast, location=top.location); }
| 'If' x1::EmptyNewlines e::MainExpr_c x2::EmptyNewlines
  'Then' x3::EmptyNewlines th::StmtList_c x4::EmptyNewlines 'End'
  { top.ast = ifStmt(e.ast, th.ast, noop(location=top.location),
                     location=top.location); }
| 'Return' e::Expr_c
  { top.ast = returnStmt(e.ast, location=top.location); }
| 'Print' e::Expr_c
  { top.ast = printStmt(e.ast, location=top.location); }
| 'Write' e::Expr_c 'to' file::Expr_c
  { top.ast = writeStmt(e.ast, file.ast, location=top.location); }
| 'Read' e::Expr_c 'to' var::UpperId_t
  { top.ast = readStmt(e.ast, var.lexeme, location=top.location); }



closed nonterminal Parse_c layout {Spacing_t, Comment_t}
   with ast<ParseDecl>, location;

concrete productions top::Parse_c
| 'Parse' x1::EmptyNewlines nt::LowerQName_t x2::EmptyNewlines 'as'
  x3::EmptyNewlines var::UpperId_t x3::EmptyNewlines
  'from' x4::EmptyNewlines e::MainExpr_c
  { top.ast = parse(toQName(nt, location=nt.location), var.lexeme,
                    e.ast, location=top.location); }



closed nonterminal DeriveRelation_c layout {Spacing_t, Comment_t}
   with ast<Derive>, location;

concrete productions top::DeriveRelation_c
| result::UpperID_t ':=' j::Judgment_c
  { top.ast = deriveRelation(result.lexeme, j.ast,
                             location=top.location); }



closed nonterminal MainExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal AndExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal CompExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal AddExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal MultExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal FactorExpr_c layout {Spacing_t, Comment_t}
   with ast<Expr>, location;
closed nonterminal Args_c layout {Spacing_t, Comment_t};

concrete productions top::MainExpr_c
| e1::MainExpr_c '||' e2::AndExpr_c
  { top.ast = orExpr(e1.ast, e2.ast, location=top.location); }
| e::AndExpr_c
  { top.ast = e.ast; }

concrete productions top::AndExpr_c
| e1::AndExpr_c '&&' e2::CompExpr_c
  { top.ast = andExpr(e1.ast, e2.ast, location=top.location); }
| e::CompExpr_c
  { top.ast = e.ast; }

concrete productions top::CompExpr_c
| e1::CompExpr_c '<' e2::AddExpr_c
  { top.ast = ltExpr(e1.ast, e2.ast, location=top.location); }
| e1::CompExpr_c '>' e2::AddExpr_c
  { top.ast = gtExpr(e1.ast, e2.ast, location=top.location); }
| e1::CompExpr_c '<=' e2::AddExpr_c
  { top.ast = leExpr(e1.ast, e2.ast, location=top.location); }
| e1::CompExpr_c '>=' e2::AddExpr_c
  { top.ast = geExpr(e1.ast, e2.ast, location=top.location); }
| e::AddExpr_c
  { top.ast = e.ast; }

concrete productions top::AddExpr_c
| e1::AddExpr_c '+' e2::MultExpr_c
  { top.ast = PlusExpr(e1.ast, e2.ast, location=top.location); }
| e1::AddExpr_c '-' e2::MultExpr_c
  { top.ast = minusExpr(e1.ast, e2.ast, location=top.location); }
| e::MultExpr_c
  { top.ast = e.ast; }

concrete productions top::MultExpr_c
| e1::MultExpr_c '*' e2::FactorExpr_c
  { top.ast = multExpr(e1.ast, e2.ast, location=top.location); }
| e1::MultExpr_c '/' e2::FactorExpr_c
  { top.ast = divExpr(e1.ast, e2.ast, location=top.location); }
| e1::MultExpr_c '%' e2::FactorExpr_c
  { top.ast = modExpr(e1.ast, e2.ast, location=top.location); }
| e::FactorExpr_c
  { top.ast = e.ast; }

concrete productions top::FactorExpr_c
| name::UpperId_t
  { top.ast = varExpr(name.lexeme, location=top.location); }
| num::Int_t
  { top.ast = intExpr(toInteger(num.lexeme), location=top.location); }
| name::LowerId_t '(' args::Args_c ')'
  { top.ast = funCall(name.lexeme, args.ast, location=top.location); }
| name::LowerId_t '(' ')'
  { top.ast = funCall(name.lexeme, nilArgs(location=top.location),
                      location=top.location); }
| '?success'
  { top.ast = successExpr(location=top.location); }
| '?failure'
  { top.ast = failureExpr(location=top.location); }
| '(' e::MainExpr_c ')'
  { top.ast = e.ast; }

concrete productions top::Args_c
| e::Expr_c
  { top.ast = oneArgs(e.ast, location=top.location); }
| e::Expr_c ',' rest::Args_c
  { top.ast = branchArgs(oneArgs(e.ast, location=e.location),
                         rest.ast, location=top.location); }
