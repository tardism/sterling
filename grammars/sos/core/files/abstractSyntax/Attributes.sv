grammar sos:core:files:abstractSyntax;


synthesized attribute pp::String;
synthesized attribute pp_comma::String;
synthesized attribute pp_space::String;


--Errors, warnings, whatever
monoid attribute errors::[Message] with [], ++;


inherited attribute moduleName::QName;

--Environments to pass things down
inherited attribute tyEnv::Env<TypeEnvItem>;
inherited attribute constructorEnv::Env<ConstructorEnvItem>;
inherited attribute judgmentEnv::Env<JudgmentEnvItem>;
inherited attribute translationEnv::Env<TranslationEnvItem>;

--Pass things up to build the environment
synthesized attribute tyDecls::[TypeEnvItem];
synthesized attribute constructorDecls::[ConstructorEnvItem];
synthesized attribute judgmentDecls::[JudgmentEnvItem];
synthesized attribute translationDecls::[TranslationEnvItem];
synthesized attribute buildsOnDecls::[QName];

--Turn a list-like thing into a list
synthesized attribute toList<a>::[a];
--Length of list-like things
synthesized attribute len::Integer;


synthesized attribute name::QName;


synthesized attribute type::Type;
synthesized attribute types::TypeList;
--
inherited attribute downSubst::Substitution;
synthesized attribute upSubst::Substitution;
inherited attribute finalSubst::Substitution;


--We need to pass around the types of variable names as we generate
--them so all occurrences of the same var have the same type
inherited attribute downVarTypes::[(String, Type)];
synthesized attribute upVarTypes::[(String, Type)];


synthesized attribute isExtensible::Boolean;

