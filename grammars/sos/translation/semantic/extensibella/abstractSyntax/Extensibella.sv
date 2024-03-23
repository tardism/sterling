grammar sos:translation:semantic:extensibella:abstractSyntax;


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





--relations used by rules
synthesized attribute usedRels::[String];
--relation being defined
synthesized attribute definedRel::String;


nonterminal Defs with pp, usedRels;

abstract production oneDefs
top::Defs ::= d::Def
{
  top.pp = d.pp ++ ".\n";

  top.usedRels = d.usedRels;
}


abstract production addDefs
top::Defs ::= d::Def rest::Defs
{
  top.pp = d.pp ++ ";\n" ++ rest.pp;

  top.usedRels = union(d.usedRels, rest.usedRels);
}





nonterminal Def with pp, usedRels, definedRel;

abstract production factDef
top::Def ::= clausehead::Metaterm
{
  top.pp = clausehead.pp;

  top.usedRels = [];
  top.definedRel = clausehead.definedRel;
}


abstract production ruleDef
top::Def ::= clausehead::Metaterm body::Metaterm
{
  top.pp = clausehead.pp ++ " := " ++ body.pp;

  top.usedRels = body.usedRels;
  top.definedRel = clausehead.definedRel;
}





--Replace a variable
inherited attribute replaceVar::String;
inherited attribute replaceVal::ExtensibellaTerm;
synthesized attribute replaced<a>::a;


nonterminal Metaterm with
   pp, vars,
   usedRels, definedRel,
   replaceVar, replaceVal, replaced<Metaterm>;

abstract production relationMetaterm
top::Metaterm ::= rel::String args::[ExtensibellaTerm]
{
  top.pp = rel ++ " " ++
           implode(" ",
              map(\ t::ExtensibellaTerm -> "(" ++ t.pp ++ ")",
                  args));

  top.vars = unions(map((.vars), args));
  --is_list and is_pair get the sub-relations added to the rel string,
  --so split the relation to find all the relations
  local parseRels::([String] ::= String) =
      \ s::String ->
        if startsWith("(", s)
        then --remove () and try again
             parseRels(substring(1, length(s) - 1, s))
        else case explode(" ", s) of
             | [] -> error("Impossible") --must be at least one
             | [r] -> [r] --simply one relation
             | r::tl -> --get rels from all sub-rels
               r::flatMap(parseRels, tl)
             end;
  top.usedRels = parseRels(rel);
  top.definedRel = rel;

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
  top.usedRels = [];
  top.definedRel =
      error("trueMetaterm.definedRel should not be accessed");

  top.replaced = top;
}


abstract production falseMetaterm
top::Metaterm ::=
{
  top.pp = "false";

  top.vars = [];
  top.usedRels = [];
  top.definedRel =
      error("falseMetaterm.definedRel should not be accessed");

  top.replaced = top;
}


abstract production eqMetaterm
top::Metaterm ::= t1::ExtensibellaTerm t2::ExtensibellaTerm
{
  top.pp = "(" ++ t1.pp ++ ") = (" ++ t2.pp ++ ")";

  top.vars = union(t1.vars, t2.vars);
  top.usedRels = [];
  top.definedRel =
      error("eqMetaterm.definedRel should not be accessed");

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

  top.vars = union(t1.vars, t2.vars);
  top.usedRels = union(t1.usedRels, t2.usedRels);
  top.definedRel =
      error("impliesMetaterm.definedRel should not be accessed");

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

  top.vars = union(t1.vars, t2.vars);
  top.usedRels = union(t1.usedRels, t2.usedRels);
  top.definedRel =
      error("andMetaterm.definedRel should not be accessed");

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
  top.usedRels = body.usedRels;
  top.definedRel =
      error("existsMetaterm.definedRel should not be accessed");

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

  top.vars = unions(map((.vars), args));

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


abstract production consExtensibellaTerm
top::ExtensibellaTerm ::= hd::ExtensibellaTerm tl::ExtensibellaTerm
{
  top.pp = "(" ++ hd.pp ++ ")::(" ++ tl.pp ++ ")";

  top.vars = union(hd.vars, tl.vars);

  hd.replaceVar = top.replaceVar;
  tl.replaceVar = top.replaceVar;
  hd.replaceVal = top.replaceVal;
  tl.replaceVal = top.replaceVal;
  top.replaced = consExtensibellaTerm(hd.replaced, tl.replaced);
}


abstract production nilExtensibellaTerm
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
  return foldr(consExtensibellaTerm, nilExtensibellaTerm(),
               charTerms);
}
function ordinalToCharConstructor
String ::= ord::Integer
{
  return "$c_" ++ toString(ord);
}





nonterminal ExtensibellaType with pp, vars;

abstract production extensibellaVarTy
top::ExtensibellaType ::= name::String
{
  top.pp = name;
  top.vars = [name];
}


abstract production extensibellaArrowTy
top::ExtensibellaType ::= ty1::ExtensibellaType ty2::ExtensibellaType
{
  top.pp = "(" ++ ty1.pp ++ ") -> " ++ ty2.pp;
  top.vars = ty1.vars ++ ty2.vars;
}


abstract production extensibellaNameTy
top::ExtensibellaType ::= name::String
{
  top.pp = name;
  top.vars = [];
}


abstract production extensibellaIntTy
top::ExtensibellaType ::=
{
  top.pp = "$lib__integer";
  top.vars = [];
}


abstract production extensibellaStringTy
top::ExtensibellaType ::=
{
  top.pp = "list $char";
  top.vars = [];
}


abstract production extensibellaListTy
top::ExtensibellaType ::= ty::ExtensibellaType
{
  top.pp = "list (" ++ ty.pp ++ ")";
  top.vars = ty.vars;
}


abstract production extensibellaPairTy
top::ExtensibellaType ::= a::ExtensibellaType b::ExtensibellaType
{
  top.pp = "$lib__pair (" ++ a.pp ++ ") (" ++ b.pp ++ ")";
  top.vars = a.vars ++ b.vars;
}





function buildExtensibellaFile
String ::= kinds::[KindDecl] constrs::[ConstrDecl]
   jdgs::[(String, [ExtensibellaType])] rules::[(String, [Def])]
   instanDefs::[Def] --instantiated rules, which come after real ones
   finalDefs::[Def] --rules that go at the end (unknown constructors)
{
  local expandedDefs::[(String, Def)] =
      flatMap(\ p::(String, [Def]) -> --reverse to maintain expected order
                map(\ d::Def -> (p.1, d), reverse(p.2)), rules);
  --group it by the relation being defined
  local relDefGroups::[[(String, Def)]] =
      groupBy(\ p1::(String, Def) p2::(String, Def) ->
                p1.2.definedRel == p2.2.definedRel,
         sortBy(\ p1::(String, Def) p2::(String, Def) ->
                  p1.2.definedRel < p2.2.definedRel,
                expandedDefs));
  --[(relation name, all def clauses)]
  local defRules::[(String, [Def])] = --relations with rules
      map(\ l::[(String, Def)] ->
            let rel::String = head(l).2.definedRel
            in
              (rel, map(snd, l) ++
                    filter(\ d::Def -> d.definedRel == rel, instanDefs) ++
                    filter(\ d::Def -> d.definedRel == rel, finalDefs))
            end,
          relDefGroups);
  local undefinedRels::[String] =
      let l::[String] = map(fst, defRules)
      in
        map(fst, filter(\ p::(String, [ExtensibellaType]) ->
                          !contains(p.1, l),
                        jdgs))
      end;
  local basicRules::[(String, [Def])] =
      defRules ++ --defined relations
      map(\ rel::String -> --and relations without basic rules
            (rel, filter(\ d::Def -> d.definedRel == rel, instanDefs) ++
                  filter(\ d::Def -> d.definedRel == rel, finalDefs)),
          undefinedRels);
  --gather up the dependencies to do the ordering
  --use jdgs so we kept rule-less rels too
  --[(relation name, names of relations it uses)]
  local dependencies::[(String, [String])] =
      map(\ p::(String, [ExtensibellaType]) ->
            (p.1, remove(p.1, flatMap(flatMap((.usedRels), _),
                                 lookupAll(p.1, basicRules)))),
          jdgs);
  --relations in order, including mutually-recursive groups
  local order::[[String]] = orderRelations(dependencies);

  local defs::[Definition] =
      map(\ l::[String] -> --list of mutual induction
            let deflist::[Def] =
                flatMap(snd,
                   filter(\ p::(String, [Def]) -> contains(p.1, l),
                          basicRules))
            in
            let defjdgs::[(String, [ExtensibellaType])] =
                filter(\ p::(String, [ExtensibellaType]) ->
                         contains(p.1, l), jdgs)
            in
              definition(defjdgs,
                 if null(deflist) --e.g. projection rels in host
                 then oneDefs(
                         ruleDef(
                            relationMetaterm(head(defjdgs).1,
                               map(\ x::ExtensibellaType ->
                                     varExtensibellaTerm(
                                        "X" ++ toString(genInt())),
                                   head(defjdgs).2)),
                            falseMetaterm()))
                 else foldrLastElem(addDefs, oneDefs, deflist))
            end end,
          order);
  return
     implode("", map((.pp), kinds)) ++ "\n\n" ++
     implode("", map((.pp), constrs)) ++ "\n\n" ++
     implode("", map((.pp), defs));
}


function buildExtensibellaInterfaceFile
String ::= modName::String buildsOns::[(String, [String])]
{
  return
     case lookup(modName, buildsOns) of
     | just(l) ->
       implode("\n",
          map(\ mod::String ->
                mod ++ " [" ++
                implode(", ", lookup(mod, buildsOns).fromJust) ++ "]",
              l))
     | nothing() ->
       error("buildExtensibellaInterfaceFile could not find " ++
             "module " ++ modName)
     end;
}



--go through a list of judgments and constructors to fill in the
--default rules of the judgments for the constructors
function instantiateExtensibellaDefaultRules
[Def] ::= newRuleComs::[(JudgmentEnvItem, [ConstructorEnvItem])]
   ebDefaultRules::[(JudgmentEnvItem, Metaterm, [Metaterm], String)]
{
  local headJdg::JudgmentEnvItem = head(newRuleComs).1;
  local newConstrs::[ConstructorEnvItem] = head(newRuleComs).2;

  --get default rule
  local defaultRule::Maybe<(Metaterm, [Metaterm], String)> =
      lookupBy(\ j1::JudgmentEnvItem j2::JudgmentEnvItem ->
                 j1.name == j2.name,
               headJdg, ebDefaultRules);
  local conc::Metaterm = defaultRule.fromJust.1;
  local prems::[Metaterm] = defaultRule.fromJust.2;
  local pc::String = defaultRule.fromJust.3;

  --get vars
  local used_vars::[String] = flatMap((.vars), prems) ++ conc.vars;
  local pcless_vars::[String] = remove(pc, used_vars);

  return
     case newRuleComs of
     | [] -> []
     | _::rest ->
       case defaultRule of
       | just(_) ->
         instantiateExtensibellaDefaultRules_help(
            newConstrs, conc, prems, pc, pcless_vars)
       | nothing() -> [] --if PC type is new, no default rule
       end ++
       instantiateExtensibellaDefaultRules(rest, ebDefaultRules)
     end;
}
--go through the list of constructors to instantiate a particular
--default rule for it
function instantiateExtensibellaDefaultRules_help
[Def] ::= constrs::[ConstructorEnvItem] conc::Metaterm
          prems::[Metaterm] pc::String pcless_vars::[String]
{
  local c::ConstructorEnvItem = head(constrs);
  --build new term
  local childNames::[String] =
      foldr(\ x::Type rest::[String] ->
              freshNameFromType(x, rest ++ pcless_vars)::rest,
            [], c.types.toList);
  local tm::ExtensibellaTerm =
      applicationExtensibellaTerm(
         decorate c.name with {
            constructorEnv = error("Not needed");
         }.ebConstructorName,
         map(varExtensibellaTerm, childNames));
  --build new rule
  local newConc::Metaterm =
      decorate conc with {
         replaceVar = pc;
         replaceVal = tm;
      }.replaced;
  local newPrems::[Metaterm] =
      map(\ m::Metaterm ->
            decorate m with {
               replaceVar = pc;
               replaceVal = tm;
            }.replaced, prems);
  local newPremVars::[String] =
      removeAll(newConc.vars,
         unions(map((.vars), newPrems)));
  local finalDef::Def =
      if null(newPrems)
      then factDef(newConc)
      else if null(newPremVars)
      then ruleDef(newConc, foldr1(andMetaterm, newPrems))
      else ruleDef(newConc, existsMetaterm(newPremVars,
                               foldr1(andMetaterm, newPrems)));
  --walk through all constructors
  return case constrs of
         | [] -> []
         | _::rest ->
           finalDef::instantiateExtensibellaDefaultRules_help(
                        rest, conc, prems, pc, pcless_vars)
         end;
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

function freshNameFromType
String ::= ty::Type used::[String]
{
  local base::String =
      case ty of
      | intType() -> "I"
      | stringType() -> "S"
      | nameType(q) -> capitalize(q.base)
      | listType(_) -> "L"
      | tupleType(x) -> if x.len == 2 then "P" else "T"
      | errorType() -> "X"
      end;
  return freshName(base, used);
}

function freshName
String ::= base::String used::[String]
{
  local reservedWords::[String] =
      ["Module", "Close", "CoDefine", "Define", "Kind", "Query",
       "Quit", "Set", "Show", "Split", "Theorem", "Type", "Prove"];
  return if contains(base, reservedWords ++ used)
         then freshName_help(base, 1, reservedWords ++ used)
         else base;
}
function freshName_help
String ::= base::String index::Integer used::[String]
{
  return if contains(base ++ toString(index), used)
         then freshName_help(base, index + 1, used)
         else base ++ toString(index);
}
