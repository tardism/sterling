grammar sos:translation:prolog;


imports sos:core:files:abstractSyntax;


--Prolog translation of a construct
synthesized attribute prolog<a>::a;


--judgments need both prolog formulas and terms
synthesized attribute prologTerm::PrologTerm;


--variable name for PC in a translation rule
synthesized attribute pcVar::String;


--Pass up translation rules
--[(judgment being defined, PC var name, premises, conclusion)]
synthesized attribute
   prologTranslationRules::[(JudgmentEnvItem, String,
                             Maybe<PrologFormula>, PrologTerm)];
inherited attribute
   prologTranslationRules_down::[(JudgmentEnvItem, String,
                                  Maybe<PrologFormula>, PrologTerm)];


--Pass up all rules so we can output them in groups by name
--[(judgment being defined, premises, conclusion)]
synthesized attribute
   prologRules::[(QName, Maybe<PrologFormula>, PrologTerm)];



--Replace vars in Prolog things
inherited attribute replaceVar::String;
inherited attribute replaceVal::PrologTerm;
synthesized attribute replaced<a>::a;

