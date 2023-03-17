grammar sos:translation:semantic:extensibella;

import silver:util:graph as g;
--need this because one graph function returns a set here
import silver:util:treemap as tm;

type Node = [String];

--put the relations in an order so all dependencies come earlier
--input is [(relation name, relations it uses)]
--output is sets of mutually-inductive relations
function orderRelations
[[String]] ::= deps::[(String, [String])]
{
  --all the edges to go into the graph
  local allEdges::[(Node, Node)] =
      flatMap(\ p::(String, [String]) ->
                map(\ x::String -> ([p.1], [x]), p.2),
              deps);
  local g::g:Graph<Node> =
      g:add(allEdges, g:emptyWith(compareNodes));
  return error("orderRelations");
}


function eqNodes
Boolean ::= l1::Node l2::Node
{
  return compareNodes(l1, l2) == 0;
}
function compareNodes
Integer ::= l1::Node l2::Node
{
  return if subset(l1, l2)
         then if subset(l2, l1)
              then 0
              else -1
         else 1; --could be incomparable, but need an answer
}
function subset
Boolean ::= sub::[String] super::[String]
{
  return all(map(\ x::String -> contains(x, super), sub));
}


--Build a spanning forest for a graph
--https://en.wikipedia.org/wiki/Spanning_tree#Spanning_forests
function spanningForest
[Tree] ::= g::g:Graph<Node>
{
  return error("spanningForest");
}
function spanningTree
Tree ::= currNode::Node g::g:Graph<Node> seen::[Node]
{
  local children::[Node] = tm:toList(g:edgesFrom(currNod, g));
  local childTrees::TreeList =
      foldr(\ child::Node rest::TreeList ->
              consTreeList(
                 spanningTree(child, g, unionBy(eqNodes, seen,
                                                rest.nodes)),
                 rest),
            emptyTreeList(), children);

  return error("spanningTree");
}


--https://en.wikipedia.org/wiki/Bridge_%28graph_theory%29




nonterminal Tree with nodes;
nonterminal TreeList with nodes;

synthesized attribute nodes::[Node];

abstract production tree
top::Tree ::= node::Node children::TreeList
{
  top.nodes = node::children.nodes;
}

abstract production emptyTreeList
top::Tree ::=
{
  top.nodes = [];
}

abstract production consTreeList
top::Tree ::= t::Tree rest::TreeList
{
  top.nodes = t.nodes ++ rest.nodes;
}
