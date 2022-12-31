grammar sos:translation:semantic:extensibella;


synthesized attribute vars::[String];


nonterminal KindDecl with pp;

abstract production kindDecl
top::KindDecl ::= name::String --everything is just `type`
{
  top.pp = "Kind " ++ name ++ "   type.\n";
}





nonterminal ConstrDecl with pp;

abstract production constrDecl
top::ConstrDecl ::= name::String args::[ExtensibellaType]
                    produced::ExtensibellaType
{
  top.pp = "Type " ++ name ++ "   " ++
           foldr(extensibellaArrowTy, produced, args).pp ++ ".\n";
}





nonterminal Definition with pp;

abstract production definition
top::Definition ::= jdgs::[(String, [ExtensibellaType])] rules::Defs
{
  top.pp = "Define " ++
      implode(",\n",
         map(\ p::(String, [ExtensibellaType]) ->
               p.1 ++ " : " ++
               foldr(extensibellaArrowTy,
                     extensibellaNameTy("prop"), p.2).pp,
             jdgs)) ++
      " by\n" ++ rules.pp;
}





nonterminal Defs with pp;

abstract production oneDefs
top::Defs ::= d::Def
{
  top.pp = d.pp ++ ".\n";
}


abstract production addDefs
top::Defs ::= d::Def rest::Defs
{
  top.pp = d.pp ++ ";\n" ++ rest.pp;
}





nonterminal Def with pp;

abstract production factDef
top::Def ::= clausehead::Metaterm
{
  top.pp = clausehead.pp;
}


abstract production ruleDef
top::Def ::= clausehead::Metaterm body::Metaterm
{
  top.pp = clausehead.pp ++ " := " ++ body.pp;
}





--Replace a variable
inherited attribute replaceVar::String;
inherited attribute replaceVal::ExtensibellaTerm;
synthesized attribute replaced<a>::a;


nonterminal Metaterm with
   pp, vars,
   replaceVar, replaceVal, replaced<Metaterm>;

abstract production relationMetaterm
top::Metaterm ::= rel::String args::[ExtensibellaTerm]
{
  top.pp = rel ++ " " ++
           implode(" ",
              map(\ t::ExtensibellaTerm -> "(" ++ t.pp ++ ")",
                  args));

  top.vars = nub(flatMap((.vars), args));

  top.replaced =
      relationMetaterm(rel,
         map(\ t::ExtensibellaTerm ->
               decorate t with {
                  replaceVar = top.replaceVar;
                  replaceVal = top.replaceVal;
               }.replaced,
             args));
}


abstract production trueMetaterm
top::Metaterm ::=
{
  top.pp = "true";

  top.vars = [];

  top.replaced = top;
}


abstract production falseMetaterm
top::Metaterm ::=
{
  top.pp = "false";

  top.vars = [];

  top.replaced = top;
}


abstract production eqMetaterm
top::Metaterm ::= t1::ExtensibellaTerm t2::ExtensibellaTerm
{
  top.pp = "(" ++ t1.pp ++ ") = (" ++ t2.pp ++ ")";

  top.vars = nub(t1.vars ++ t2.vars);

  t1.replaceVar = top.replaceVar;
  t2.replaceVar = top.replaceVar;
  t1.replaceVal = top.replaceVal;
  t2.replaceVal = top.replaceVal;
  top.replaced = eqMetaterm(t1.replaced, t2.replaced);
}


abstract production impliesMetaterm
top::Metaterm ::= t1::Metaterm t2::Metaterm
{
  top.pp = "(" ++ t1.pp ++ ") -> (" ++ t2.pp ++ ")";

  top.vars = nub(t1.vars ++ t2.vars);

  t1.replaceVar = top.replaceVar;
  t2.replaceVar = top.replaceVar;
  t1.replaceVal = top.replaceVal;
  t2.replaceVal = top.replaceVal;
  top.replaced = impliesMetaterm(t1.replaced, t2.replaced);
}


abstract production andMetaterm
top::Metaterm ::= t1::Metaterm t2::Metaterm
{
  top.pp = "(" ++ t1.pp ++ ") /\\ (" ++ t2.pp ++ ")";

  top.vars = nub(t1.vars ++ t2.vars);

  t1.replaceVar = top.replaceVar;
  t2.replaceVar = top.replaceVar;
  t1.replaceVal = top.replaceVal;
  t2.replaceVal = top.replaceVal;
  top.replaced = andMetaterm(t1.replaced, t2.replaced);
}


abstract production existsMetaterm
top::Metaterm ::= names::[String] body::Metaterm
{
  top.pp = "exists " ++ implode(" ", names) ++ ", " ++ body.pp;

  top.vars = body.vars;

  body.replaceVar = top.replaceVar;
  body.replaceVal = top.replaceVal;
  top.replaced =
      if contains(top.replaceVar, names)
      then top
      else existsMetaterm(names, body.replaced);
}





nonterminal ExtensibellaTerm with
   pp, vars,
   replaceVar, replaceVal, replaced<ExtensibellaTerm>;

abstract production applicationExtensibellaTerm
top::ExtensibellaTerm ::= name::String args::[ExtensibellaTerm]
{
  top.pp = name ++ " " ++
           implode(" ", map(\ t::ExtensibellaTerm ->
                              "(" ++ t.pp ++ ")", args));

  top.vars = nub(flatMap((.vars), args));

  top.replaced =
      applicationExtensibellaTerm(name,
         map(\ t::ExtensibellaTerm ->
               decorate t with {
                  replaceVar = top.replaceVar;
                  replaceVal = top.replaceVal;
               }.replaced, args));
}


abstract production nameExtensibellaTerm
top::ExtensibellaTerm ::= name::String
{
  top.pp = name;

  top.vars = [];

  top.replaced = nameExtensibellaTerm(name);
}


abstract production varExtensibellaTerm
top::ExtensibellaTerm ::= name::String
{
  top.pp = name;

  top.vars = [name];

  top.replaced =
      if name == top.replaceVar
      then top.replaceVal
      else top;
}


abstract production consTerm
top::ExtensibellaTerm ::= hd::ExtensibellaTerm tl::ExtensibellaTerm
{
  top.pp = "(" ++ hd.pp ++ ")::(" ++ tl.pp ++ ")";

  top.vars = nub(hd.vars ++ tl.vars);

  hd.replaceVar = top.replaceVar;
  tl.replaceVar = top.replaceVar;
  hd.replaceVal = top.replaceVal;
  tl.replaceVal = top.replaceVal;
  top.replaced = consTerm(hd.replaced, tl.replaced);
}


abstract production nilTerm
top::ExtensibellaTerm ::=
{
  top.pp = "nil";

  top.vars = [];

  top.replaced = top;
}


--Functions for producing special terms
function extensibellaIntegerTerm
ExtensibellaTerm ::= i::Integer
{
  local baseNum::Integer = if i < 0 then -i - 1 else i;
  local numTrans::ExtensibellaTerm =
      foldr(\ s::String rest::ExtensibellaTerm ->
              applicationExtensibellaTerm(s, [rest]),
            nameExtensibellaTerm("$zero"), repeat("$succ", baseNum));
  return
     if i >= 0
     then applicationExtensibellaTerm("$posInt", [numTrans])
     else applicationExtensibellaTerm("$negSuccInt", [numTrans]);
}

function extensibellaStringTerm
ExtensibellaTerm ::= s::String
{
  local charOrdinals::[Integer] = stringToChars(s);
  local charConstants::[String] =
      map(ordinalToCharConstructor, charOrdinals);
  local charTerms::[ExtensibellaTerm] =
      map(nameExtensibellaTerm(_), charConstants);
  return foldr(consTerm, nilTerm(), charTerms);
}
function ordinalToCharConstructor
String ::= ord::Integer
{
  return "$c_" ++ toString(ord);
}





nonterminal ExtensibellaType with pp;

abstract production extensibellaArrowTy
top::ExtensibellaType ::= ty1::ExtensibellaType ty2::ExtensibellaType
{
  top.pp = "(" ++ ty1.pp ++ ") -> " ++ ty2.pp;
}


abstract production extensibellaNameTy
top::ExtensibellaType ::= name::String
{
  top.pp = name;
}


abstract production extensibellaIntTy
top::ExtensibellaType ::=
{
  top.pp = "$lib__integer";
}


abstract production extensibellaStringTy
top::ExtensibellaType ::=
{
  top.pp = "list $char";
}





function buildExtensibellaFile
String ::= kinds::[KindDecl] constrs::[ConstrDecl]
   jdgs::[(String, [ExtensibellaType])] rules::[(String, [Def])]
{
  {-
    We sort these because we want the rules to be in a consestent
    order even if they are imported by two different modules, then
    a third module including both of those.  This is necessary for
    putting the proofs together right.

    All the rules within each module set should always end up in the
    same order in the list.
  -}
  local sortedRules::[(String, [Def])] =
      sortBy(\ p1::(String, [Def]) p2::(String, [Def]) ->
               p1.1 < p2.1, rules);
  local basicRules::[Def] =
      flatMap(\ p::(String, [Def]) -> p.2, sortedRules);
  local defs::Definition =
      definition(jdgs,
         foldr(\ d::Def rest::Defs -> addDefs(d, rest),
               oneDefs(last(basicRules)),
               take(length(basicRules) - 1, basicRules)));
  return
     implode("", map((.pp), kinds)) ++ "\n\n" ++
     implode("", map((.pp), constrs)) ++ "\n\n" ++
     defs.pp;
}





function capitalize
String ::= name::String
{
  local first::String =
        case substring(0, 1, name) of
        | "a" -> "A" | "b" -> "B" | "c" -> "C" | "d" -> "D" | "e" -> "E"
        | "f" -> "F" | "g" -> "G" | "h" -> "H" | "i" -> "I" | "j" -> "J"
        | "k" -> "K" | "l" -> "L" | "m" -> "M" | "n" -> "N" | "o" -> "O"
        | "p" -> "P" | "q" -> "Q" | "r" -> "R" | "s" -> "S" | "t" -> "T"
        | "u" -> "U" | "v" -> "V" | "w" -> "W" | "x" -> "X" | "y" -> "Y"
        | "z" -> "Z" | x -> x
        end;
  return first ++ substring(1, length(name), name);
}

function freshName
String ::= base::String used::[String]
{
  return if contains(base, used)
         then freshName_help(base, 1, used)
         else base;
}
function freshName_help
String ::= base::String index::Integer used::[String]
{
  return if contains(base ++ toString(index), used)
         then freshName_help(base, index + 1, used)
         else base ++ toString(index);
}
