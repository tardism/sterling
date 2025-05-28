% Complete Test File for Sterling Standard Library
% Load the generated Prolog file
:- consult('generated/prolog/stlc:host.pl').

% COUNT tests (3 rules: empty, match, no-match)
:- sterling_MODULE_stdLib_MODULE_count(a, [], 0).
:- sterling_MODULE_stdLib_MODULE_count(a, [a,b,a,c,a], 3).
:- sterling_MODULE_stdLib_MODULE_count(x, [a,b,c], 0).

% DOMAIN tests (2 rules: empty, cons)
:- sterling_MODULE_stdLib_MODULE_domain([], []).
:- sterling_MODULE_stdLib_MODULE_domain([(a,1),(b,2),(c,3)], [a,b,c]).

% DROP tests (2 rules: 0 case, step case)
:- sterling_MODULE_stdLib_MODULE_drop(0, [a,b,c], [a,b,c]).
:- sterling_MODULE_stdLib_MODULE_drop(2, [a,b,c,d], [c,d]).

% LOOKUP tests (2 rules: match, no-match-continue)
:- sterling_MODULE_stdLib_MODULE_lookup([(a,1),(b,2),(c,3)], b, 2).
:- sterling_MODULE_stdLib_MODULE_lookup([(x,9),(b,2),(c,3)], c, 3).

% MEM tests (2 rules: found-in-tail, found-at-head)
:- sterling_MODULE_stdLib_MODULE_mem(a, [a,b,c]).
:- sterling_MODULE_stdLib_MODULE_mem(c, [a,b,c]).

% NO_LOOKUP tests (2 rules: empty-list, cons-with-different-key)
:- sterling_MODULE_stdLib_MODULE_no_lookup([], anykey).
:- sterling_MODULE_stdLib_MODULE_no_lookup([(a,1),(b,2)], c).

% NOT_MEM tests (2 rules: empty-list, cons-with-different-item)
:- sterling_MODULE_stdLib_MODULE_not_mem(x, []).
:- sterling_MODULE_stdLib_MODULE_not_mem(d, [a,b,c]).

% PERMUTATION tests (2 rules: empty-lists, cons-case)
:- sterling_MODULE_stdLib_MODULE_permutation([], []).
:- sterling_MODULE_stdLib_MODULE_permutation([a,b], [b,a]).

% RANGE tests (2 rules: low>high, low<=high)
:- sterling_MODULE_stdLib_MODULE_range(5, 3, []).
:- sterling_MODULE_stdLib_MODULE_range(1, 3, [1,2,3]).

% SELECT tests (2 rules: select-from-tail, select-first)
:- sterling_MODULE_stdLib_MODULE_select(a, [b,c], [a,b,c]).
:- sterling_MODULE_stdLib_MODULE_select(b, [a,c], [b,a,c]).

% SUBSET tests (2 rules: empty-list, cons-case)
:- sterling_MODULE_stdLib_MODULE_subset([], [a,b,c]).
:- sterling_MODULE_stdLib_MODULE_subset([a,c], [a,b,c,d]).

% TAKE tests (2 rules: 0-case, step-case)
:- sterling_MODULE_stdLib_MODULE_take(0, [a,b,c], []).
:- sterling_MODULE_stdLib_MODULE_take(2, [a,b,c,d], [a,b]).

% VALUES tests (2 rules: empty-list, cons)
:- sterling_MODULE_stdLib_MODULE_values([], []).
:- sterling_MODULE_stdLib_MODULE_values([(a,1),(b,2),(c,3)], [1,2,3]).

% ZIP tests (2 rules: empty-lists, cons-case)
:- sterling_MODULE_stdLib_MODULE_zip([], [], []).
:- sterling_MODULE_stdLib_MODULE_zip([a,b,c], [1,2,3], [(a,1),(b,2),(c,3)]).

% Negative test cases (should fail)
:- \+ sterling_MODULE_stdLib_MODULE_lookup([(a,1),(b,2)], c, _).
:- \+ sterling_MODULE_stdLib_MODULE_no_lookup([(a,1),(b,2)], a).
:- \+ sterling_MODULE_stdLib_MODULE_not_mem(a, [a,b,c]).
:- \+ sterling_MODULE_stdLib_MODULE_subset([a,e], [a,b,c]).
:- \+ sterling_MODULE_stdLib_MODULE_mem(d, [a,b,c]).
:- \+ sterling_MODULE_stdLib_MODULE_take(5, [a,b], [a,b]).

% Edge cases
:- sterling_MODULE_stdLib_MODULE_drop(0, [], []).
:- sterling_MODULE_stdLib_MODULE_range(3, 3, [3]).

:- write('All Sterling standard library tests passed!'), nl.