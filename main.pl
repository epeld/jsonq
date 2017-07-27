
:- use_module(library(http/json)).

main :-
  read_query(Query),
  read_root(Root),

  forall(run_query(Root, Query, Result),
         (
             output_stream(Output),
             json_write(Output, Result),
             format("~n")
         )).


read_query(Query) :-
  read_string("list(Value), property(Key, Value), sought(Value).", Query).

read_root(Root) :-
  json_read_string("[1,2,false, {\"foo\":\"bar\",\"baz\":[33]}]", Root).

read_root2(Root) :-
  input_stream(Input),
  json_read(Input, Root).


input_stream(S) :-
  stream_property(S, alias(user_input)).


output_stream(S) :-
  stream_property(S, alias(user_output)).


command_line_args(Args) :-
  current_prolog_flag(argv, Args).


read_string(String, Term) :-
  open_string(String, Stream),
  call_cleanup(read(Stream, Term),
               close(Stream)).



json_read_string(String, Term) :-
  open_string(String, Stream),
  call_cleanup(json_read(Stream, Term),
               close(Stream)).
