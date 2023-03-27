grammar sos:core:semanticDefs:concreteSyntax;


--How far we are into a list
inherited attribute index::Integer;


closed nonterminal File_c layout {Spacing_t, Comment_t}
   with ast<File>, location;

concrete productions top::File_c
| name::ModuleDecl_c x::EmptyNewlines d::DeclList_c
  { top.ast = file(name.ast, d.ast, location=top.location); }



closed nonterminal DeclList_c layout {Spacing_t, Comment_t}
   with ast<Decls>, location;

concrete productions top::DeclList_c
|
  { top.ast = nilDecls(location=top.location); }
| d::TopDecl_c x::EmptyNewlines rest::DeclList_c
  { top.ast = branchDecls(d.ast, rest.ast, location=top.location); }



closed nonterminal TopDecl_c layout {Spacing_t, Comment_t}
   with ast<Decls>, location;

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



closed nonterminal JudgmentDecl_c layout {Spacing_t, Comment_t}
   with ast<JudgmentDecl>, location;

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



closed nonterminal TypeList_c layout {Spacing_t, Comment_t}
   with ast<TypeList>, location;

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



closed nonterminal AbsSyntaxDecl_c layout {Spacing_t, Comment_t}
   with ast<AbsSyntaxDecl>, location;

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



closed nonterminal AbsConstructorDecls_c layout {Spacing_t, Comment_t}
   with ast<AbsConstructorDecls>, location;

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



closed nonterminal AbsConstructorDecl_c layout {Spacing_t, Comment_t}
   with ast<AbsConstructorDecls>, location;

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




closed nonterminal Rule_c layout {Spacing_t, Comment_t}
   with ast<Rule>, location;

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



{-New productions added to either Judgment_c or JudgmentArbitrarySpace_c
  should have a corresponding production added to the other-}
closed nonterminal Judgment_c layout {Spacing_t, Comment_t}
   with ast<Judgment>, location;
--Productions for JudgmentArbitrarySpace_c should end with EmptyNewlines
closed nonterminal JudgmentArbitrarySpace_c layout {Spacing_t, Comment_t}
   with ast<Judgment>, location;

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
{-
  Sometimes judgments get really long and you need to split across
  lines.  However, the whitespace-sensitive nature of our language
  means you can't do that.  So we have this grouping construct using
  curly braces allowing arbitrary line breaks in a judgment.
-}
| '{' x1::EmptyNewlines j::JudgmentArbitrarySpace_c '}'
  { top.ast = j.ast; }

concrete productions top::JudgmentArbitrarySpace_c
| '!' x2::EmptyNewlines rel::LowerId_t x3::EmptyNewlines
  args::TermListArbitrarySpace_c
  { top.ast = negationRelation(toQName(rel.lexeme, rel.location),
                               args.ast, location=top.location); }
| '!' x2::EmptyNewlines rel::LowerQName_t x3::EmptyNewlines
  args::TermListArbitrarySpace_c
  { top.ast = negationRelation(toQName(rel.lexeme, rel.location),
                               args.ast, location=top.location); }
| '!' x2::EmptyNewlines rel::LowerId_t x3::EmptyNewlines
  { top.ast = negationRelation(toQName(rel.lexeme, rel.location),
                               nilTermList(location=top.location),
                               location=top.location); }
| '!' x2::EmptyNewlines rel::LowerQName_t x3::EmptyNewlines
  { top.ast = negationRelation(toQName(rel.lexeme, rel.location),
                               nilTermList(location=top.location),
                               location=top.location); }
| rel::LowerId_t x2::EmptyNewlines args::TermListArbitrarySpace_c
  { top.ast = relation(toQName(rel.lexeme, rel.location),
                       args.ast, location=top.location); }
| rel::LowerQName_t x2::EmptyNewlines args::TermListArbitrarySpace_c
  { top.ast = relation(toQName(rel.lexeme, rel.location),
                       args.ast, location=top.location); }
| rel::LowerId_t x::EmptyNewlines
  { top.ast = relation(toQName(rel.lexeme, rel.location),
                       nilTermList(location=top.location),
                       location=top.location); }
| rel::LowerQName_t x::EmptyNewlines
  { top.ast = relation(toQName(rel.lexeme, rel.location),
                       nilTermList(location=top.location),
                       location=top.location); }
| t1::Term_c x2::EmptyNewlines op::TopBinOp_c x3::EmptyNewlines
  t2::Term_c x4::EmptyNewlines
  { top.ast = topBinOpJudgment(t1.ast, op.ast, t2.ast,
                               location=top.location); }
| '|{' ty::LowerId_t '}-' x2::EmptyNewlines
   t1::Term_c x3::EmptyNewlines '~~>' x4::EmptyNewlines t2::Term_c
   x5::EmptyNewlines
  { top.ast = transJudgment(nilTermList(location=top.location),
                            toQName(ty.lexeme, ty.location),
                            t1.ast, t2.ast,
                            location=top.location); }
| '|{' ty::LowerQName_t '}-' x2::EmptyNewlines
  t1::Term_c x3::EmptyNewlines '~~>' x4::EmptyNewlines t2::Term_c
  x5::EmptyNewlines
  { top.ast = transJudgment(nilTermList(location=top.location),
                            toQName(ty.lexeme, ty.location),
                            t1.ast, t2.ast,
                            location=top.location); }
| args::ContainedCommaTermList_c
  '|{' ty::LowerId_t '}-' x3::EmptyNewlines t1::Term_c
  x4::EmptyNewlines '~~>' x5::EmptyNewlines t2::Term_c
  x6::EmptyNewlines
  { top.ast = transJudgment(args.ast, toQName(ty.lexeme, ty.location),
                            t1.ast, t2.ast,
                            location=top.location); }
| args::ContainedCommaTermList_c
  '|{' ty::LowerQName_t '}-' x3::EmptyNewlines t1::Term_c
  x4::EmptyNewlines '~~>' x5::EmptyNewlines t2::Term_c
  x6::EmptyNewlines
  { top.ast = transJudgment(args.ast, toQName(ty.lexeme, ty.location),
                            t1.ast, t2.ast,
                            location=top.location); }
| t1::Term_c x2::EmptyNewlines op::BinOp_c
  x3::EmptyNewlines t2::Term_c x4::EmptyNewlines '='
  x5::EmptyNewlines t3::Term_c x6::EmptyNewlines
  { top.ast = binOpJudgment(t1.ast, op.ast, t2.ast, t3.ast,
                            location=top.location); }



closed nonterminal BinOp_c layout {Spacing_t, Comment_t}
   with ast<BinOp>, location;

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



closed nonterminal TopBinOp_c layout {Spacing_t, Comment_t}
   with ast<TopBinOp>, location;

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



closed nonterminal JudgmentList_c layout {Spacing_t, Comment_t}
   with ast<JudgmentList>, location;

concrete productions top::JudgmentList_c
|
  { top.ast = nilJudgmentList(location=top.location); }
              -- 1+ lines between
| j::Judgment_c Newline_t x::EmptyNewlines rest::JudgmentList_c
  { top.ast = consJudgmentList(j.ast, rest.ast,
                               location=top.location); }



closed nonterminal Term_c layout {Spacing_t, Comment_t}
   with ast<Term>, location;

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
| constructor::LowerId_t
  '(' x1::EmptyNewlines args::ContainedCommaTermList_c ')'
  { top.ast =
        appTerm(toQName(constructor.lexeme,
                        constructor.location),
                args.ast, location=top.location); }
| constructor::LowerQName_t
  '(' x1::EmptyNewlines args::ContainedCommaTermList_c ')'
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
  {-
    Tuples are delimited by '(|' and '|)' because using only
    parentheses creates ambiguities with constructors:  We can't tell
    whether "x (1, 2)" is supposed to be a single term, a constructor
    x with arguments 1 and 2, or two terms, a constructor x without
    arguments and a tuple.
  -}
| '(|' x1::EmptyNewlines '|)'
  { top.ast = tupleTerm(nilTermList(location=top.location),
                        location=top.location); }
| '(|' x1::EmptyNewlines contents::ContainedCommaTermList_c '|)'
  { top.ast =
        case contents.ast.toList of
        | [t] -> t
        | _ -> tupleTerm(contents.ast, location=top.location)
        end; }
| '[' x1::EmptyNewlines ']'
  { top.ast = nilTerm(location=top.location); }
| '[' x1::EmptyNewlines contents::ContainedCommaTermList_c ']'
  { top.ast = contents.listTerm; }
| hd::Term_c '::' x1::EmptyNewlines tl::Term_c
  { top.ast = consTerm(hd.ast, tl.ast, location=top.location); }
  {-
    Ascription uses curly braces because other delimiters we might use
    cause ambiguities.  We could use bananas like tuples, but that
    seems likely to confuse because it isn't a tuple, it is a single
    term for which we are specifying the type.
  -}
| '{' x1::EmptyNewlines t::Term_c x2::EmptyNewlines ':'
      x3::EmptyNewlines ty::Type_c x4::EmptyNewlines '}'
  { top.ast = ascriptionTerm(t.ast, ty.ast, location=top.location); }



closed nonterminal TermList_c layout {Spacing_t, Comment_t}
   with ast<TermList>, location;

concrete productions top::TermList_c
| t::Term_c
  { top.ast = consTermList(t.ast, nilTermList(location=top.location),
                           location=top.location); }
| t::Term_c rest::TermList_c
  { top.ast = consTermList(t.ast, rest.ast, location=top.location); }



--Allows arbitrary line breaks in a term list
closed nonterminal TermListArbitrarySpace_c layout {Spacing_t, Comment_t}
   with ast<TermList>, location;

concrete productions top::TermListArbitrarySpace_c
| t::Term_c x::EmptyNewlines
  { top.ast = consTermList(t.ast, nilTermList(location=top.location),
                           location=top.location); }
| t::Term_c x::EmptyNewlines rest::TermListArbitrarySpace_c
  { top.ast = consTermList(t.ast, rest.ast, location=top.location); }



closed nonterminal CommaTermList_c layout {Spacing_t, Comment_t}
   with ast<TermList>, location;

concrete productions top::CommaTermList_c
| t::Term_c
  { top.ast = consTermList(t.ast, nilTermList(location=top.location),
                           location=top.location); }
--Because we require the comma to be on the same line, we can let the
--user put in extra newlines to organize things nicer without making
--the grammar ambiguous
| t::Term_c ',' x::EmptyNewlines rest::CommaTermList_c
  { top.ast = consTermList(t.ast, rest.ast, location=top.location); }



--Ends with empty newlines because it is followed by a delimiter
closed nonterminal ContainedCommaTermList_c
   layout {Spacing_t, Comment_t}
   with ast<TermList>, listTerm, location;
--turn the contents of a list into a list term with cons
synthesized attribute listTerm::Term;

concrete productions top::ContainedCommaTermList_c
| t::Term_c x::EmptyNewlines
  { top.ast = consTermList(t.ast, nilTermList(location=top.location),
                           location=top.location);
    top.listTerm = consTerm(t.ast, nilTerm(location=top.location),
                            location=top.location); }
--Because we require the comma to be on the same line, we can let the
--user put in extra newlines to organize things nicer without making
--the grammar ambiguous
| t::Term_c ',' x::EmptyNewlines rest::ContainedCommaTermList_c
  { top.ast = consTermList(t.ast, rest.ast, location=top.location);
    top.listTerm = consTerm(t.ast, rest.listTerm,
                            location=top.location); }
