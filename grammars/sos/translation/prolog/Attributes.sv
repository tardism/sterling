grammar sos:translation:prolog;


imports sos:core:files:abstractSyntax;


--Prolog translation of a construct
synthesized attribute prolog<a>::a;


--judgments need both prolog formulas and terms
synthesized attribute prologTerm::PrologTerm;


--variable name for PC in a translation rule
synthesized attribute pcVar::String;


--Pass up translation rules
--[(judgment being defined, PC var name, premises, conclusion)]
synthesized attribute
   prologTranslationRules::[(JudgmentEnvItem, String,
                             Maybe<PrologFormula>, PrologTerm)];
inherited attribute
   prologTranslationRules_down::[(JudgmentEnvItem, String,
                                  Maybe<PrologFormula>, PrologTerm)];


--Pass up all rules so we can output them in groups by name
--[(judgment being defined, premises, conclusion)]
synthesized attribute
   prologRules::[(QName, Maybe<PrologFormula>, PrologTerm)];



--Replace vars in Prolog things
inherited attribute replaceVar::String;
inherited attribute replaceVal::PrologTerm;
synthesized attribute replaced<a>::a;


--Get a count of the number of occurrences of each variable
synthesized attribute countVarOccurrences::[(String, Integer)];

function combineVarOccurrences
[(String, Integer)] ::= l1::[(String, Integer)] l2::[(String, Integer)]
{
  return
     case l1 of
     | [] -> l2
     | (v, count)::rest ->
       case lookup(v, l2) of
       | nothing() -> (v, count)::combineVarOccurrences(rest, l2)
       | just(count2) ->
         --remove the count of v from the rest
         let removed::[(String, Integer)] =
             filter(\ p::(String, Integer) -> p.1 != v, l2)
         in
         --combine the rest
         let combinedRest::[(String, Integer)] =
             combineVarOccurrences(rest, removed)
         in
           (v, count + count2)::combinedRest
         end end
       end
     end;
}

