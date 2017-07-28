
%
% Top Level Interface
%
run_query(Root, Query, Result) :-
  compile_query(Query, CompiledQuery),
  run_compiled_query(Root, CompiledQuery, Result).

%
% Query Compilation
%

compile_query(CompoundTerm, Query) :-
  clauses_to_list(CompoundTerm, ClauseList),
  rearrange_clauses(ClauseList, Query).


clauses_to_list(Clauses, [Clause | OtherClauses]) :-
  functor(Clauses, ',', 2),
  !,
  arg(1, Clauses, Clause),
  arg(2, Clauses, Clauses2),
  clauses_to_list(Clauses2, OtherClauses).

clauses_to_list(Clause, [Clause]) :-
  \+ functor(Clause, ',', 2).


rearrange_clauses(ClauseList, SortedList) :-
  partition(structure_clause, ClauseList, Structure, Rest),
  append(Structure, Rest, SortedList).


structure_clause(Clause) :-
  clause(structure(Clause, _Root), _Body).

%
% Query Execution
%

run_compiled_query(Root, [Pred | Rest], Result) :-
  run(Root, Pred, Result),
  run_compiled_query(Root, Rest, Result).

run_compiled_query(_Root, [], Result) :-
  ground(Result).


% Run structure predicates
run(Root, Pred1, _Result) :-
  structure(Pred1, Root).

% Run type predicates
run(_Root, Pred1, _Result) :-
  ground(Pred1), % To avoid errors
  type(Pred1).

% Special 'pragma':
run(_Root, sought(Result), Result).

% For calling whitelisted standard prolog predicates
run(_Root, Pred, _Result) :-
  whitelist(RealName/Arity, PredName/Arity),
  Pred =.. [PredName | Args],
  RealPred =.. [RealName | Args],
  call(RealPred).


%
% Structure Queries
%
structure(root(Root), Root).

structure(value(Value), Root) :-
  property(Root, _Key, Value).

structure(key(Key), Root) :-
  property(Root, Key, _Value).

structure(property(Key, Value), Root) :-
  property(Root, Key, Value).

%
% Type Queries
%
type(object(Object)) :-
  object(Object).

type(list(List)) :-
  list(List).

type(number(Number)) :-
  number(Number).

type(boolean(Bool)) :-
  boolean(Bool).

type(string(String)) :-
  atom(String).


%
% Type Predicates
%

object(json(_)).

list([]).
list([_X | _]).

boolean(@false).
boolean(@true).

%
% Whitelist standard predicates
%

whitelist(between/3).
whitelist(member/2).
whitelist(nth0/3).
whitelist(nth1/3).

whitelist(atom_prefix/2, string_prefix/2).

whitelist(Term/Arity, Term/Arity) :-
  whitelist(Term/Arity).




%
% 
%

% Special case for root element:
property(Element, "root", Element).

property(Element, Key, Value) :-
  subproperty(Element, Key, Value).

subproperty(json(Pairs), Key, Value) :-
  member(Key = Value, Pairs).

subproperty(json(Pairs), Key, Value) :-
  member(_Key = SubTree, Pairs),
  subproperty(SubTree, Key, Value).

subproperty([X | Xs], Key, Value) :-
  nth0(Key, [X | Xs], Value).

subproperty([X | Xs], Key, Value) :-
  member(Item, [X | Xs]),
  subproperty(Item, Key, Value).

