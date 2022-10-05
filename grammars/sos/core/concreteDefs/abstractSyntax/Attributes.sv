grammar sos:core:concreteDefs:abstractSyntax;

imports sos:core:common:abstractSyntax;


--Environment to pass things down
inherited attribute concreteEnv::Env<ConcreteEnvItem>;


--Pass things up to build the environment
synthesized attribute concreteDecls::[ConcreteEnvItem];


synthesized attribute typeList::[Type];


--Expected argument types
--just() if something is expected, nothing() in an error condition
inherited attribute expectedTypes::Maybe<[Type]>;
synthesized attribute remainingTypes::Maybe<[Type]>;
--
inherited attribute argumentIndex::Integer;
synthesized attribute nextArgumentIndex::Integer;
--
inherited attribute lastConstructor::QName;


inherited attribute productionElements::[(String, QName, Type, Location)];

synthesized attribute gatherProdElems::[(String, QName, Type, Location)];
