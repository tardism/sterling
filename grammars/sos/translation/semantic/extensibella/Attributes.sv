grammar sos:translation:semantic:extensibella;

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

synthesized attribute ebRulesByModule::[(String, [Def])];


--Gather judgment names and types
synthesized attribute ebJudgments::[(String, [ExtensibellaType])];


--Variable for the PC in a translation rule's conclusion
synthesized attribute pcVar::String;
