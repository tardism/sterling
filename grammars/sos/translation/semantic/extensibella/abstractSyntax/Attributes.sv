grammar sos:translation:semantic:extensibella:abstractSyntax;

imports sos:core:common:abstractSyntax;
imports sos:core:semanticDefs:abstractSyntax;


--Extensibella translation of a construct
synthesized attribute eb<a>::a;


--Gather syntax declarations
synthesized attribute ebKinds::[KindDecl];
synthesized attribute ebConstrs::[ConstrDecl];


--Gather rules
synthesized attribute ebRules::[Def];
--Gather translation rules [(judgment, conclusion, premises, PC var)]
synthesized attribute ebTranslationRules::[(JudgmentEnvItem, Metaterm,
                                            [Metaterm], String)];
synthesized attribute ebStandInRules::[(JudgmentEnvItem, Metaterm,
                                        [Metaterm], String)];

synthesized attribute ebRulesByModule::[(String, [Def])];


--Gather judgment names and types
synthesized attribute ebJudgments::[(String, [ExtensibellaType])];


--Errors arising from translating to Extensibella that are not
--problems otherwise
synthesized attribute ebErrors::[String];


--Variable for the PC in a translation rule's conclusion
synthesized attribute pcVar::String;


--is relation for a type
synthesized attribute ebIs::String;


--whether all arguments to a relation are variables
synthesized attribute allArgsVars::Boolean;
--whether a term is a variable
synthesized attribute isVar::Boolean;
