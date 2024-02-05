grammar sos:translation:semantic:extensibella:abstractSyntax;

import silver:util:graph as g;
--need this because one graph function returns a set here
import silver:util:treeset as ts;


--put the relations in an order so all dependencies come earlier
--input is [(relation name, relations it uses)]
--output is sets of mutually-inductive relations
function orderRelations
[[String]] ::= deps::[(String, [String])]
{
  --Filter out any relations in the dependencies not defined here
  local rels::[String] = map(fst, deps);
  local filteredDeps::[(String, [String])] =
      map(\ p::(String, [String]) ->
            (p.1, filter(contains(_, rels), p.2)),
          deps);
  --All the edges to go into the graph
  local allEdges::[(String, String)] =
      flatMap(\ p::(String, [String]) ->
                map(\ x::String -> (p.1, x), p.2),
              filteredDeps);
  local g::g:Graph<String> = g:add(allEdges, g:empty());

  --Build strongly-connected components
  --Conveniently, these are already topologically sorted in reverse
  --   order as well
  local comps::[[String]] = kosaraju(g, rels);

  return reverse(comps);
}




--Find strongly-connected components
--These will become mutual definitions
--https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
function kosaraju
[[String]] ::= g::g:Graph<String> graphNodes::[String]
{
  --build the stack by iterating the nodes because it might not be
  --connected, so starting with a single node isn't enough
  local stack::[String] =
      foldr(\ node::String rest::[String] ->
              if contains(node, rest)
              then rest
              else kosaraju_buildStack(node, g, rest, [node]),
            [], graphNodes);

  --build the transpose, the graph with the edges reversed
  local newEdges::[(String, String)] =
      map(\ p::(String, String) -> (p.2, p.1), g:toList(g));
  local transpose::g:Graph<String> = g:add(newEdges, g:empty());

  --build the strongly-connected components
  local comps::[[String]] =
      kosaraju_iterateStack(stack, transpose, []);
  return comps;
}

--build the stack for node order
function kosaraju_buildStack
[String] ::= currNode::String g::g:Graph<String> thusFar::[String]
             seen::[String]
{
  local children::[String] = ts:toList(g:edgesFrom(currNode, g));
  --build the stack by iterating through children
  local outStack::[String] =
      currNode::foldr(\ child::String rest::[String] ->
                        if contains(child, rest ++ seen)
                        then rest
                        else kosaraju_buildStack(child, g,
                                rest, child::seen),
                      thusFar, children);
  return outStack;
}

--go through the stack and build components
function kosaraju_iterateStack
[[String]] ::= stack::[String] g::g:Graph<String> seen::[String]
{
  local hereComp::[String] =
      kosaraju_buildComponent(head(stack), g, [], seen);
  return case stack of
         | [] -> []
         | h::t ->
           if contains(h, seen)
           then kosaraju_iterateStack(tail(stack), g, seen)
           else hereComp::kosaraju_iterateStack(tail(stack), g,
                                                hereComp ++ seen)
         end;
}
--Build a single component
--thusFar is the current component
function kosaraju_buildComponent
[String] ::= currNode::String g::g:Graph<String> thusFar::[String]
             alreadyDone::[String]
{
  local children::[String] = ts:toList(g:edgesFrom(currNode, g));
  return
      foldr(\ child::String rest::[String] ->
              if contains(child, rest) ||
                 contains(child, alreadyDone)
              then rest
              else kosaraju_buildComponent(child, g, rest,
                      alreadyDone),
            currNode::thusFar, children);
}
