%% @author Kevin Smith <ksmith@basho.com>
%% @copyright 2009-2010 Basho Technologies
%%
%% SPDX-FileCopyrightText: 2009-2010 Basho Technologies
%% SPDX-FileCopyrightText: 2020-2026 Peter Lemenkov <lemenkov@gmail.com>
%% SPDX-License-Identifier: Apache-2.0

%% @doc This module manages all of the low-level details surrounding the
%% NIF library. It is responsible for creating and destroying
%% instances of Javascript VMs.

-module(js_driver).

-define(DEFAULT_HEAP_SIZE, 8). %% MB
-define(DEFAULT_THREAD_STACK, 16). %% MB
-define(DEFAULT_TIMEOUT, 5000).

-export([new/0, new/2, new/3, destroy/1]).
-export([define_js/2, define_js/3, eval_js/2, eval_js/3]).

%% @doc Create a new Javascript VM instance and preload Douglas Crockford's
%% json2 converter. Uses a default heap size of 8MB and a default thread
%% stack size of 16MB.
-spec new() -> {ok, reference()} | {error, atom()} | {error, any()}.
new() ->
    new(?DEFAULT_THREAD_STACK, ?DEFAULT_HEAP_SIZE).

%% @doc Create a new Javascript VM instance and preload Douglas Crockford's
%% json2 converter.
-spec new(pos_integer(), pos_integer()) -> {ok, reference()} | {error, atom()} | {error, any()}.
new(ThreadStackSize, HeapSize) ->
    Initializer = fun(X) -> define_js(X, <<"json2.js">>) end,
    new(ThreadStackSize, HeapSize, Initializer).

%% @doc Create a new Javascript VM instance. The function arguments control
%% how the VM instance is initialized. User supplied initializers must
%% return true or false.
-spec new(pos_integer(), pos_integer(), function() | {atom(), atom()}) ->
    {ok, reference()} | {error, atom()} | {error, any()}.
new(ThreadStackSize, HeapSize, Initializer) when is_function(Initializer) ->
    {ok, Port} = mozjs_nif:sm_init(ThreadStackSize, HeapSize),
    case Initializer(Port) of
        ok ->
            {ok, Port};
        {error, Error} ->
            js_driver:destroy(Port),
            error_logger:error_report(Error),
            throw({error, init_failed})
    end;
new(ThreadStackSize, HeapSize, {InitMod, InitFun}) ->
    Initializer = fun(X) -> InitMod:InitFun(X) end,
    new(ThreadStackSize, HeapSize, Initializer).

%% @doc Destroys a Javascript VM instance.
-spec destroy(reference()) -> ok.
destroy(Ctx) ->
    mozjs_nif:sm_stop(Ctx).

%% @doc Define a Javascript expression.
-spec define_js(reference(), binary() | {binary(), binary()} | {file, string()}) ->
    ok | {error, any()}.
define_js(Ctx, JsSrc) ->
    exec_js(Ctx, JsSrc, no_jsonify, 0, ?DEFAULT_TIMEOUT).

%% @doc Define a Javascript expression with a timeout.
-spec define_js(reference(), binary() | {binary(), binary()} | {file, string()}, timeout()) ->
    ok | {error, any()}.
define_js(Ctx, JsSrc, Timeout) ->
    exec_js(Ctx, JsSrc, no_jsonify, 0, Timeout).

%% @doc Evaluate a Javascript expression and return the result.
-spec eval_js(reference(), binary() | {binary(), binary()} | {file, string()}) ->
    {ok, any()} | {error, any()}.
eval_js(Ctx, JsSrc) ->
    exec_js(Ctx, JsSrc, jsonify, 1, ?DEFAULT_TIMEOUT).

%% @doc Evaluate a Javascript expression with a timeout and return the result.
-spec eval_js(reference(), binary() | {binary(), binary()} | {file, string()}, timeout()) ->
    {ok, any()} | {error, any()}.
eval_js(Ctx, JsSrc, Timeout) ->
    exec_js(Ctx, JsSrc, jsonify, 1, Timeout).

%% Internal functions
%% @private
jsonify(Code) when is_binary(Code) ->
    {Body, <<LastChar:8>>} = split_binary(Code, size(Code) - 1),
    C =
        case LastChar of
            $; ->
                Body;
            _ ->
                Code
        end,
    list_to_binary([<<"JSON.stringify(">>, C, <<");">>]).

%% @private
exec_js(Ctx, {file, FileName}, Jsonify, HandleRetval, Timeout) ->
    {ok, Js} = file:read_file(FileName),
    exec_js(Ctx, to_binary(FileName), Js, Jsonify, HandleRetval, Timeout);
exec_js(Ctx, {FileName, Js}, Jsonify, HandleRetval, Timeout) ->
    exec_js(Ctx, to_binary(FileName), to_binary(Js), Jsonify, HandleRetval, Timeout);
exec_js(Ctx, Js, Jsonify, HandleRetval, Timeout) ->
    exec_js(Ctx, <<"unnamed">>, to_binary(Js), Jsonify, HandleRetval, Timeout).

exec_js(Ctx, FileName, Js, jsonify, HandleRetval, Timeout) ->
    exec_js(Ctx, FileName, jsonify(Js), no_jsonify, HandleRetval, Timeout);
exec_js(Ctx, FileName, Js, _Jsonify, HandleRetval, Timeout) when
    is_binary(FileName), is_binary(Js)
->
    case mozjs_nif:sm_eval(Ctx, FileName, Js, HandleRetval, Timeout) of
        ok ->
            ok;
        {ok, <<"undefined">>} ->
            {error, mozjs_script_interrupted};
        {ok, Result} ->
            {ok, jsx:decode(Result)};
        {error, ErrorJson} when is_binary(ErrorJson) ->
            case jsx:decode(ErrorJson) of
                Error when is_map(Error) ->
                    {error, Error};
                _ ->
                    {error, ErrorJson}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% @private
to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L).
