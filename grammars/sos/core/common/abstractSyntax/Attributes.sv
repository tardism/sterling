grammar sos:core:common:abstractSyntax;


synthesized attribute pp::String;
--list separated by commas or by spaces
--have both rather than just using pp because some lists have both
synthesized attribute pp_comma::String;
synthesized attribute pp_space::String;


--Errors, warnings, whatever
monoid attribute errors::[Message] with [], ++;


--Environments to pass things down
inherited attribute tyEnv::Env<TypeEnvItem>;
inherited attribute constructorEnv::Env<ConstructorEnvItem>;


--Turn a list-like thing into a list
synthesized attribute toList<a>::[a];
--Length of list-like things
synthesized attribute len::Integer;


inherited attribute moduleName::QName;


synthesized attribute name::QName;


synthesized attribute type::Type;
synthesized attribute types::TypeList;


synthesized attribute isError::Boolean;



