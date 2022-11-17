grammar sos:translation:semantic:lambdaProlog;


imports sos:core:common:abstractSyntax;
imports sos:core:semanticDefs:abstractSyntax;


--Lambda Prolog translation of a construct
synthesized attribute lp<a>::a;


synthesized attribute lpDecls::[LambdaPrologDeclaration];


synthesized attribute lpRules::[LambdaPrologRule];


--judgments need both formulas and terms
synthesized attribute lpTerm::LambdaPrologTerm;


--variable name for PC in a translation rule
synthesized attribute pcVar::String;


--Pass up translation rules
--[(judgment being defined, PC var name, rule)]
synthesized attribute
   lpTranslationRules::[(JudgmentEnvItem, String, LambdaPrologRule)];



--Replace vars in Lambda Prolog things
inherited attribute replaceVar::String;
inherited attribute replaceVal::LambdaPrologTerm;
synthesized attribute replaced<a>::a;


synthesized attribute vars::[String];

