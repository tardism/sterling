grammar sos:core:main:abstractSyntax;

inherited attribute funEnv::Env<FunctionEnvItem>;
synthesized attribute funDecls::[FunctionEnvItem];

inherited attribute lastFun::QName;

inherited attribute expectedReturnType::Type;
