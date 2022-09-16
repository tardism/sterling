grammar sos:core:semanticDefs:concreteSyntax;


--How far we are into a list
inherited attribute index::Integer;


closed nonterminal File_c with ast<File>, location;

concrete productions top::File_c
| name::ModuleDecl_c x::EmptyNewlines d::DeclList_c
  { top.ast = file(name.ast, d.ast, location=top.location); }



closed nonterminal DeclList_c with ast<Decls>, location;

concrete productions top::DeclList_c
|
  { top.ast = nilDecls(location=top.location); }
| d::TopDecl_c x::EmptyNewlines rest::DeclList_c
  { top.ast = branchDecls(d.ast, rest.ast, location=top.location); }



closed nonterminal TopDecl_c with ast<Decls>, location;

concrete productions top::TopDecl_c
| 'Builds' 'on' i::LowerId_t Newline_t
  { top.ast = buildsOnDecls(toQName(i.lexeme, i.location),
                            location=top.location); }
| 'Builds' 'on' i::LowerQName_t Newline_t
  { top.ast = buildsOnDecls(toQName(i.lexeme, i.location),
                            location=top.location); }
| r::Rule_c
  { top.ast = ruleDecls(r.ast, location=top.location); }
| decl::AbsSyntaxDecl_c
  { top.ast = absSyntaxDecls(decl.ast, location=top.location); }
| decl::JudgmentDecl_c
  { top.ast = judgmentDecls(decl.ast, location=top.location); }



closed nonterminal JudgmentDecl_c with ast<JudgmentDecl>, location;

concrete productions top::JudgmentDecl_c
| 'Judgment' name::LowerId_t ':' type::TypeList_c Newline_t
  { top.ast = extJudgmentDecl(name.lexeme, type.ast,
                              location=top.location);
  }
| 'Fixed' 'Judgment' name::LowerId_t ':' type::TypeList_c Newline_t
  { top.ast = fixedJudgmentDecl(name.lexeme, type.ast,
                                location=top.location);
  }
| 'Translation' tyname::LowerId_t ':' type::TypeList_c Newline_t
  { top.ast = translationTypeDecl(tyname.lexeme, type.ast,
                                  location=top.location);
  }



closed nonterminal TypeList_c with ast<TypeList>, location;

concrete productions top::TypeList_c
|
  { top.ast = nilTypeList(location=top.location); }
| ty::Type_c rest::TypeList_c
  { top.ast = consTypeList(ty.ast, rest.ast, location=top.location); }


concrete productions top::Type_c
| var::UpperId_t
  { top.ast = varType(var.lexeme, location=top.location); }
| ty::Type_c '*'
  { top.ast = pcType(ty.ast, location=ty.location); }



closed nonterminal AbsSyntaxDecl_c with ast<AbsSyntaxDecl>, location;

concrete productions top::AbsSyntaxDecl_c
| tyName::LowerId_t '::=' decls::AbsConstructorDecls_c
  { top.ast = initialAbsSyntaxDecl(tyName.lexeme, decls.ast,
                                   location=top.location); }
| tyName::LowerId_t '::=' '|' decls::AbsConstructorDecls_c
  { top.ast = initialAbsSyntaxDecl(tyName.lexeme, decls.ast,
                                   location=top.location); }
| ty::LowerId_t '::=' '.' '.' '.' x::EmptyNewlines
                  '|' decls::AbsConstructorDecls_c
  { top.ast =
        addAbsSyntaxDecl(toQName(ty.lexeme, ty.location),
                         decls.ast, location=top.location); }
| ty::LowerQName_t '::=' '.' '.' '.' x::EmptyNewlines
                     '|' decls::AbsConstructorDecls_c
  { top.ast =
        addAbsSyntaxDecl(toQName(ty.lexeme, ty.location),
                         decls.ast, location=top.location); }



closed nonterminal AbsConstructorDecls_c with
   ast<AbsConstructorDecls>, location;

concrete productions top::AbsConstructorDecls_c
| d::AbsConstructorDecl_c Newline_t
  { top.ast = d.ast; }
| d::AbsConstructorDecl_c '|' rest::AbsConstructorDecls_c
  { top.ast = branchAbsConstructorDecls(d.ast, rest.ast,
                                        location=top.location); }
| d::AbsConstructorDecl_c Newline_t '|' rest::AbsConstructorDecls_c
  { top.ast = branchAbsConstructorDecls(d.ast, rest.ast,
                                        location=top.location); }
| Newline_t rest::AbsConstructorDecls_c
  { top.ast = rest.ast; }



closed nonterminal AbsConstructorDecl_c with
   ast<AbsConstructorDecls>, location;

concrete productions top::AbsConstructorDecl_c
| name::LowerId_t
  { top.ast =
        oneConstructorDecl(name.lexeme,
           nilTypeList(location=name.location),
           location=top.location); }
| name::LowerId_t '(' x1::EmptyNewlines args::CommaTypeList_c
                      x2::EmptyNewlines ')'
  { top.ast = oneConstructorDecl(name.lexeme, args.ast,
                                 location=top.location); }




closed nonterminal CommaTypeList_c with ast<TypeList>, location;

concrete productions top::CommaTypeList_c
| ty::Type_c
  { top.ast =
        consTypeList(ty.ast, nilTypeList(location=top.location),
                     location=top.location); }
| ty::Type_c ',' x2::EmptyNewlines rest::CommaTypeList_c
  { top.ast = consTypeList(ty.ast, rest.ast, location=top.location); }



closed nonterminal Rule_c with ast<Rule>, location;

concrete productions top::Rule_c
| premises::JudgmentList_c
  ExtLine_t '[' r::RuleName_t ']' Newline_t x::EmptyNewlines
  conclusion::Judgment_c Newline_t
  { top.ast = extRule(premises.ast, r.lexeme, conclusion.ast,
                      location=top.location); }
| premises::JudgmentList_c
  ExtLine_t '[' r::RuleName_t ']' '*' Newline_t x::EmptyNewlines
  conclusion::Judgment_c Newline_t
  { top.ast = transRule(premises.ast, r.lexeme, conclusion.ast,
                        location=top.location); }
| premises::JudgmentList_c
  FixedLine_t '[' r::RuleName_t ']' Newline_t x::EmptyNewlines
  conclusion::Judgment_c Newline_t
  { top.ast = fixedRule(premises.ast, r.lexeme, conclusion.ast,
                        location=top.location); }



closed nonterminal Judgment_c with ast<Judgment>, location;

concrete productions top::Judgment_c
| '!' rel::LowerId_t args::TermList_c
  { top.ast = negationRelation(toQName(rel.lexeme, rel.location),
                               args.ast, location=top.location); }
| '!' rel::LowerQName_t args::TermList_c
  { top.ast = negationRelation(toQName(rel.lexeme, rel.location),
                               args.ast, location=top.location); }
| '!' rel::LowerId_t
  { top.ast = negationRelation(toQName(rel.lexeme, rel.location),
                               nilTermList(location=top.location),
                               location=top.location); }
| '!' rel::LowerQName_t
  { top.ast = negationRelation(toQName(rel.lexeme, rel.location),
                               nilTermList(location=top.location),
                               location=top.location); }
| rel::LowerId_t args::TermList_c
  { top.ast = relation(toQName(rel.lexeme, rel.location),
                       args.ast, location=top.location); }
| rel::LowerQName_t args::TermList_c
  { top.ast = relation(toQName(rel.lexeme, rel.location),
                       args.ast, location=top.location); }
| rel::LowerId_t
  { top.ast = relation(toQName(rel.lexeme, rel.location),
                       nilTermList(location=top.location),
                       location=top.location); }
| rel::LowerQName_t
  { top.ast = relation(toQName(rel.lexeme, rel.location),
                       nilTermList(location=top.location),
                       location=top.location); }
| t1::Term_c op::TopBinOp_c t2::Term_c
  { top.ast = topBinOpJudgment(t1.ast, op.ast, t2.ast,
                               location=top.location); }
| '|{' ty::LowerId_t '}-' t1::Term_c x2::EmptyNewlines '~~>'
                                     x3::EmptyNewlines t2::Term_c
  { top.ast = transJudgment(nilTermList(location=top.location),
                            toQName(ty.lexeme, ty.location),
                            t1.ast, t2.ast,
                            location=top.location); }
| '|{' ty::LowerQName_t '}-' t1::Term_c x2::EmptyNewlines '~~>'
                                        x3::EmptyNewlines t2::Term_c
  { top.ast = transJudgment(nilTermList(location=top.location),
                            toQName(ty.lexeme, ty.location),
                            t1.ast, t2.ast,
                            location=top.location); }
| args::CommaTermList_c '|{' ty::LowerId_t '}-' x1::EmptyNewlines
                          t1::Term_c x2::EmptyNewlines '~~>'
                                     x3::EmptyNewlines t2::Term_c
  { top.ast = transJudgment(args.ast, toQName(ty.lexeme, ty.location),
                            t1.ast, t2.ast,
                            location=top.location); }
| args::CommaTermList_c '|{' ty::LowerQName_t '}-' x1::EmptyNewlines
                         t1::Term_c x2::EmptyNewlines '~~>'
                                    x3::EmptyNewlines t2::Term_c
  { top.ast = transJudgment(args.ast, toQName(ty.lexeme, ty.location),
                            t1.ast, t2.ast,
                            location=top.location); }
| t1::Term_c op::BinOp_c t2::Term_c '=' t3::Term_c
  { top.ast = binOpJudgment(t1.ast, op.ast, t2.ast, t3.ast,
                            location=top.location); }
| t1::Term_c '=' t2::Term_c op::BinOp_c t3::Term_c
  { top.ast = binOpJudgment(t2.ast, op.ast, t3.ast, t1.ast,
                            location=top.location); }



closed nonterminal BinOp_c with ast<BinOp>, location;

concrete productions top::BinOp_c
| '+'
  { top.ast = plusOp(location=top.location); }
| '-'
  { top.ast = minusOp(location=top.location); }
| '*'
  { top.ast = multOp(location=top.location); }
| '/'
  { top.ast = divOp(location=top.location); }
| '%'
  { top.ast = modOp(location=top.location); }
| '++'
  { top.ast = appendOp(location=top.location); }



closed nonterminal TopBinOp_c with ast<TopBinOp>, location;

concrete productions top::TopBinOp_c
| '='
  { top.ast = eqOp(location=top.location); }
| '!='
  { top.ast = neqOp(location=top.location); }
| '<'
  { top.ast = lessOp(location=top.location); }
| '>'
  { top.ast = greaterOp(location=top.location); }
| '<='
  { top.ast = leqOp(location=top.location); }
| '>='
  { top.ast = geqOp(location=top.location); }



closed nonterminal JudgmentList_c with ast<JudgmentList>, location;

concrete productions top::JudgmentList_c
|
  { top.ast = nilJudgmentList(location=top.location); }
              -- 1+ lines between
| j::Judgment_c Newline_t x::EmptyNewlines rest::JudgmentList_c
  { top.ast = consJudgmentList(j.ast, rest.ast,
                               location=top.location); }



closed nonterminal Term_c with ast<Term>, location;

concrete productions top::Term_c
| constant::LowerId_t
  { top.ast =
        const(toQName(constant.lexeme, constant.location),
              location=top.location); }
| constant::LowerQName_t
  { top.ast =
        const(toQName(constant.lexeme, constant.location),
              location=top.location); }
| varname::UpperId_t
  { top.ast = var(varname.lexeme, location=top.location); }
| int::Integer_t
  { top.ast = num(toInteger(int.lexeme), location=top.location); }
| s::String_t
  { top.ast =
        stringConst(substring(1, length(s.lexeme) - 1, s.lexeme),
                    location=top.location); }
| constructor::LowerId_t '(' x1::EmptyNewlines args::CommaTermList_c
                             x2::EmptyNewlines ')'
  { top.ast =
        appTerm(toQName(constructor.lexeme,
                        constructor.location),
                args.ast, location=top.location); }
| constructor::LowerQName_t '(' x1::EmptyNewlines args::CommaTermList_c
                                x2::EmptyNewlines ')'
  { top.ast =
        appTerm(toQName(constructor.lexeme,
                        constructor.location),
                args.ast, location=top.location); }
| constructor::LowerId_t '(' x::EmptyNewlines ')'
  { top.ast =
        appTerm(toQName(constructor.lexeme,
                        constructor.location),
                nilTermList(location=top.location),
                location=top.location); }
| constructor::LowerQName_t '(' x::EmptyNewlines ')'
  { top.ast =
        appTerm(toQName(constructor.lexeme,
                        constructor.location),
                nilTermList(location=top.location),
                location=top.location); }
| '<' x1::EmptyNewlines t::Term_c x2::EmptyNewlines ':'
      x3::EmptyNewlines ty::Type_c x4::EmptyNewlines '>'
  { top.ast = ascriptionTerm(t.ast, ty.ast, location=top.location); }



closed nonterminal TermList_c with ast<TermList>, location;

concrete productions top::TermList_c
| t::Term_c
  { top.ast = consTermList(t.ast, nilTermList(location=top.location),
                           location=top.location); }
| t::Term_c rest::TermList_c
  { top.ast = consTermList(t.ast, rest.ast, location=top.location); }



closed nonterminal CommaTermList_c with ast<TermList>, location;

concrete productions top::CommaTermList_c
| t::Term_c
  { top.ast = consTermList(t.ast, nilTermList(location=top.location),
                           location=top.location); }
--Because we require the comma to be on the same line, we can let the
--user put in extra newlines to organize things nicer without making
--the grammar ambiguous
| t::Term_c ',' x::EmptyNewlines rest::CommaTermList_c
  { top.ast = consTermList(t.ast, rest.ast, location=top.location); }

