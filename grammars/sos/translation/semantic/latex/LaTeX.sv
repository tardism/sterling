grammar sos:translation:semantic:latex;


nonterminal LaTeXTerm with ppLaTeX;

abstract production constLaTeXTerm
top::LaTeXTerm ::= const::String
{
  top.ppLaTeX = const;
}


abstract production macroLaTeXTerm
top::LaTeXTerm ::= macroName::String args::LaTeXTermList
{
  top.ppLaTeX = "\\" ++ macroName ++ args.ppLaTeX;
}





nonterminal LaTeXTermList with ppLaTeX;

abstract production nilLaTeXTermList
top::LaTeXTermList ::=
{
  top.ppLaTeX = "";
}


abstract production addLaTeXTermList
top::LaTeXTermList ::= t::LaTeXTerm rest::LaTeXTermList
{
  top.ppLaTeX = "{" ++ t.ppLaTeX ++ "}" ++ rest.ppLaTeX;
}





nonterminal LaTeXJudgment with ppLaTeX;

abstract production termLaTeXJudgment
top::LaTeXJudgment ::= t::LaTeXTerm
{
  top.ppLaTeX = t.ppLaTeX;
}


abstract production negationLaTeXJudgment
top::LaTeXJudgment ::= t::LaTeXTerm
{
  top.ppLaTeX = "\\ensuremath{\\lnot} " ++ t.ppLaTeX;
}


abstract production transLaTeXJudgment
top::LaTeXJudgment ::= macro::String args::LaTeXTermList t::LaTeXTerm
                       trans::LaTeXTerm
{
  top.ppLaTeX = "\\" ++ macro ++ args.ppLaTeX ++ "{" ++ t.ppLaTeX ++
                "}{" ++ trans.ppLaTeX ++ "}";
}


abstract production binOpLaTeXJudgment
top::LaTeXJudgment ::= t1::LaTeXTerm op::BinOp t2::LaTeXTerm
                       result::LaTeXTerm
{
  top.ppLaTeX = t1.ppLaTeX ++ " " ++ op.ppLaTeX ++ " " ++
                t2.ppLaTeX ++ " = " ++ result.ppLaTeX;
}


abstract production topBinOpLaTeXJudgment
top::LaTeXJudgment ::= t1::LaTeXTerm op::TopBinOp t2::LaTeXTerm
{
  top.ppLaTeX = t1.ppLaTeX ++ " " ++ op.ppLaTeX ++ " " ++
                t2.ppLaTeX;
}





nonterminal LaTeXJudgmentList with ppLaTeX;

abstract production nilLaTeXJudgmentList
top::LaTeXJudgmentList ::=
{
  top.ppLaTeX = "";
}


abstract production consLaTeXJudgmentList
top::LaTeXJudgmentList ::= j::LaTeXJudgment rest::LaTeXJudgmentList
{
  top.ppLaTeX = j.ppLaTeX ++
              ( if rest.ppLaTeX == ""
                then ""
                else " \\andalso\n" ++
                     "        " ++ rest.ppLaTeX );
}





nonterminal LaTeXRule with ppLaTeX;

abstract production latexRule
top::LaTeXRule ::= name::String premises::LaTeXJudgmentList
                   conclusion::LaTeXJudgment
{
  top.ppLaTeX = "\\infrule[" ++ name ++ "]\n" ++
                "        {" ++ premises.ppLaTeX ++ "}\n" ++
                "        {" ++ conclusion.ppLaTeX ++ "}\n";
}





nonterminal LaTeXAbsConstructorDecls with ppLaTeX;

abstract production nilLaTeXAbsConstructorDecls
top::LaTeXAbsConstructorDecls ::=
{
  top.ppLaTeX = "";
}


abstract production oneLaTeXAbsConstructorDecls
top::LaTeXAbsConstructorDecls ::= macro::String args::LaTeXTermList
{
  --have a LaTeX newline at the end of each constructor
  top.ppLaTeX = "\\" ++ macro ++ args.ppLaTeX ++ "\\\\";
}


abstract production branchLaTeXAbsConstructorDecls
top::LaTeXAbsConstructorDecls ::= d1::LaTeXAbsConstructorDecls
                                  d2::LaTeXAbsConstructorDecls
{
  top.ppLaTeX =
      if d1.ppLaTeX == ""
      then d2.ppLaTeX
      else if d2.ppLaTeX == ""
      then d1.ppLaTeX
      else d1.ppLaTeX ++ "\n" ++
           "\\> \\ttfamily \\textbar~" ++ d2.ppLaTeX;
}





nonterminal LaTeXAbsSyn with ppLaTeX;

abstract production initLaTeXAbsSyn
top::LaTeXAbsSyn ::= ty::String constrs::LaTeXAbsConstructorDecls
{
  top.ppLaTeX =
      "\\begin{tabbing}\n" ++
      replicate(length(ty) + 3, "x") ++ "\\= \\kill\n" ++
      "\\ttfamily " ++ ty ++ " ::= " ++ constrs.ppLaTeX ++
      "\n\\end{tabbing}\n";
}


abstract production addLaTeXAbsSyn
top::LaTeXAbsSyn ::= ty::String constrs::LaTeXAbsConstructorDecls
{
  --have a LaTeX newline (\\) in there
  top.ppLaTeX =
      "\\begin{tabbing}\n" ++
      replicate(length(ty) + 3, "x") ++ "\\= \\kill\n" ++
      "\\ttfamily " ++ ty ++ " ::= ...\\\\\n" ++
      "\\> \\ttfamily \\textbar~" ++ constrs.ppLaTeX ++
      "\n\\end{tabbing}\n";
}





--ppLaTeX on these is defining the macro
attribute ppLaTeX occurs on JudgmentEnvItem, TranslationEnvItem,
                            ConstructorEnvItem;

aspect production extJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList pcIndex::Integer
{
  --\newcommand{\rel}[n]{name(#1, #2, ... #n)}
  top.ppLaTeX = "\\newcommand{\\" ++ relMacro(name) ++ "}" ++
                "[" ++ toString(args.len) ++ "]" ++
                "{" ++ base ++ argsString ++ "}";
  local argsString::String =
        if args.len > 0
        then  "(#" ++ implode(", #",
                         map(toString, range(1, args.len + 1))) ++ ")"
        else "";
  local base::String = substitute("_", "\\_", name.base);
}


aspect production fixedJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList
{
  --\newcommand{\rel}[n]{name(#1, #2, ... #n)}
  top.ppLaTeX = "\\newcommand{\\" ++ relMacro(name) ++ "}" ++
                "[" ++ toString(args.len) ++ "]" ++
                "{" ++ base ++ argsString ++ "}";
  local argsString::String =
        if args.len > 0
        then  "(#" ++ implode(", #",
                         map(toString, range(1, args.len + 1))) ++ ")"
        else "";
  local base::String = substitute("_", "\\_", name.base);
}


aspect production errorJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList
{
  top.ppLaTeX = "";
}



aspect production translationEnvItem
top::TranslationEnvItem ::= name::QName args::TypeList
{
  --\newcommand{\rel}[n + 2]{#1, #2, ... #n |- #n+1 ~~> #n+2}
  top.ppLaTeX = "\\newcommand{\\" ++ transMacro(name) ++ "}" ++
                "[" ++ toString(args.len + 2) ++ "]" ++
                "{\\ensuremath{" ++ argsString ++
                " \\vdash_{" ++ base ++ "} " ++
                  "#" ++ toString(args.len + 1) ++
                  " \\rightsquigarrow #" ++ toString(args.len + 2) ++
                  "}}";
  local argsString::String =
        if args.len > 0
        then  "#" ++ implode(", #",
                        map(toString, range(1, args.len + 1)))
        else "";
  local base::String = substitute("_", "\\_", name.base);
}



aspect production constructorEnvItem
top::ConstructorEnvItem ::= name::QName builtType::Type args::TypeList
{
  --\newcommand{\constructor}[n]{name(#1, #2, ... #n)}
  top.ppLaTeX = "\\newcommand{\\" ++ constrMacro(name) ++ "}" ++
                "[" ++ toString(args.len) ++ "]" ++
                "{" ++ base ++ argsString ++ "}";
  local argsString::String =
        if args.len > 0
        then  "(#" ++ implode(", #",
                         map(toString, range(1, args.len + 1))) ++ ")"
        else "";
  local base::String = substitute("_", "\\_", name.base);
}
