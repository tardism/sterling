grammar sos:core:common:concreteSyntax;


synthesized attribute ast<a>::a;


closed nonterminal ModuleDecl_c with ast<QName>, location;

concrete productions top::ModuleDecl_c
| 'Module' name::LowerQName_t
{ top.ast = toQName(name.lexeme, name.location); }
| 'Module' name::LowerId_t
{ top.ast = toQName(name.lexeme, name.location); }




closed nonterminal Type_c with ast<Type>, location;

concrete productions top::Type_c
| 'int'
  { top.ast = intType(location=top.location); }
| 'string'
  { top.ast = stringType(location=top.location); }
| ty::LowerId_t
  { top.ast = nameType(toQName(ty.lexeme, ty.location),
                       location=top.location); }
| ty::LowerQName_t
  { top.ast = nameType(toQName(ty.lexeme, ty.location),
                       location=top.location); }



{-
  Because newlines are not a part of our ignore terminals and are used
  as part of the actual syntax, we need to explicitly add them in the
  places where we want to allow them but not require them.  We use
  this nonterminal to allow the extra, unneeded newlines, but also not
  require them.
-}
closed nonterminal EmptyNewlines;

concrete productions top::EmptyNewlines
|
  { }
| Newline_t rest::EmptyNewlines
  { }

