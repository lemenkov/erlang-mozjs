%% @author Kevin Smith <ksmith@basho.com>
%% @copyright 2009-2010 Basho Technologies
%%
%% SPDX-FileCopyrightText: 2009-2010 Basho Technologies
%% SPDX-FileCopyrightText: 2020-2026 Peter Lemenkov <lemenkov@gmail.com>
%% SPDX-License-Identifier: Apache-2.0

%% @doc Convenience module for interacting with Javascript from Erlang.
%% The functions provided by this module marshal bindings and function
%% args into JSON before sending them to Javascript. While this does
%% incur a certain amount of overhead it has the benefit of (mostly)
%% preserving types as they roundtrip between Erlang and Javascript.
%% Of course, this also means all Erlang values MUST BE convertable
%% into JSON. In practice, this is less restricting than it sounds.
-module(js).

-export([define/2, define/3, eval/2, call/3, call/4]).

%% @doc Define one or more Javascript expressions.
-spec define(reference(), binary()) -> ok | {error, any()}.
define(Ctx, Js) ->
    define(Ctx, Js, []).

%% @doc Define one or more Javascript expressions using a set of bindings.
%% Bindings are useful when the expressions use closures.
-spec define(reference(), binary(), list({binary() | atom(), any()})) -> ok | {error, any()}.
define(Ctx, Js, Bindings) ->
    JsBindings = list_to_binary(build_bindings(Bindings, [])),
    FinalJs = iolist_to_binary([JsBindings, Js]),
    js_driver:define_js(Ctx, FinalJs).

%% @doc Evaluate one or more Javascript expressions and return the results.
-spec eval(reference(), binary()) -> {ok, any()} | {error, any()}.
eval(Ctx, Js) ->
    js_driver:eval_js(Ctx, Js).

%% @doc Call a function by name with a list of arguments.
%% This is roughly the same as apply in most other languages.
-spec call(reference(), binary(), list(any())) -> {ok, any()} | {error, any()}.
call(Ctx, FunctionName, Args) ->
    call(Ctx, FunctionName, Args, []).

%% @doc Call a function by name with a list of arguments and environmental
%% bindings. Bindings behave just like define/3.
-spec call(reference(), binary(), list(any()), list({binary() | atom(), any()})) ->
    {ok, any()} | {error, any()}.
call(Ctx, FunctionName, Args, Bindings) ->
    JsBindings = list_to_binary(build_bindings(Bindings, [])),
    ArgList = build_arg_list(Args, []),
    EscapedFunctionName = binary:replace(FunctionName, <<"\"">>, <<"\\\"">>, [global]),
    Js = iolist_to_binary([
        <<"function() {">>,
        JsBindings,
        <<" if (">>,
        FunctionName,
        <<" === undefined) { throw(\"">>,
        EscapedFunctionName,
        <<" not defined\"); } ">>,
        <<"return ">>,
        FunctionName,
        <<"(">>,
        ArgList,
        <<");">>,
        <<"}();">>
    ]),
    js_driver:eval_js(Ctx, Js).

%% Internal functions
build_bindings([], Accum) ->
    Accum;
build_bindings([{VarName, Value} | T], Accum) ->
    FinalVarName =
        case is_atom(VarName) of
            true ->
                atom_to_list(VarName);
            false ->
                VarName
        end,
    build_bindings(T, [[FinalVarName, "=", jsx:encode(Value), ";"] | Accum]).

build_arg_list([], Accum) ->
    lists:reverse(Accum);
build_arg_list([H | []], Accum) ->
    build_arg_list([], [jsx:encode(H) | Accum]);
build_arg_list([H | T], Accum) ->
    build_arg_list(T, [[jsx:encode(H), ","] | Accum]).
