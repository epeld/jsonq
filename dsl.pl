
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


%
% Query Execution
%

run_compiled_query(Root, Query, Result) :-
  foldl(run_(Root, Result), Query, [], PostPoned),
  
  ground(Result),
  
  (
    PostPoned = [] *->
    true
  ;
  format("Warning: some predicates could never be run ~w ~w~n", [Result, PostPoned])
  ).


% Argument flipping:
run_(Root, Result, Predicate, P, P2) :-
  run(Root, Predicate, Result, P, P2).


% Run structure predicates
run(Root, Pred1, _Result, PostPoned, PostPoned2) :-
  structure(Pred1, Root),
  partition(ground, PostPoned, Types, PostPoned2),
  
  % Run all postponed type predicates
  forall(member(Type, Types), type(Type)).

% Run type predicates
run(_Root, Pred1, _Result, PostPoned, PostPoned2) :-
  ground(Pred1) *->
    type(Pred1), PostPoned2 = PostPoned
  ; % Postpone the predicate because it needs to be ground first
    clause(type(Pred1), _Body), PostPoned2 = [Pred1 | PostPoned].


run(_Root, sought(Result), Result, PostPoned, PostPoned).


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

