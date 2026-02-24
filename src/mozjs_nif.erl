%% SPDX-FileCopyrightText: 2009-2010 Basho Technologies
%% SPDX-FileCopyrightText: 2020-2026 Peter Lemenkov <lemenkov@gmail.com>
%% SPDX-License-Identifier: Apache-2.0

-module(mozjs_nif).

-export([
    sm_init/2,
    sm_eval/4,
    sm_eval/5,
    sm_cancel/1,
    sm_stop/1
]).

-on_load(init/0).

-define(nif_stub, erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

init() ->
    %% Use the application name (mozjs), not the module name (mozjs_nif),
    %% so that code:priv_dir/1 finds the correct path under rebar3's
    %% _build/default/lib/mozjs/priv/ layout.
    PrivDir =
        case code:priv_dir(mozjs) of
            {error, bad_name} ->
                EbinDir = filename:dirname(code:which(?MODULE)),
                AppPath = filename:dirname(EbinDir),
                filename:join(AppPath, "priv");
            Path ->
                Path
        end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

sm_init(_ThreadStack, _HeapSize) ->
    ?nif_stub.

sm_eval(Ref, Filename, Js, HandleRetval) when is_list(Filename) ->
    sm_eval(Ref, list_to_binary(Filename), Js, HandleRetval);
sm_eval(Ref, Filename, Js, HandleRetval) when is_list(Js) ->
    sm_eval(Ref, Filename, list_to_binary(Js), HandleRetval);
sm_eval(Ref, Filename, Js, HandleRetval) ->
    sm_eval(Ref, Filename, Js, HandleRetval, 0).

sm_eval(Ref, Filename, Js, HandleRetval, 0) ->
    sm_eval_nif(Ref, Filename, Js, HandleRetval);
sm_eval(Ref, Filename, Js, HandleRetval, Timeout) ->
    Pid = self(),
    spawn(
        fun() ->
            Ret = (catch sm_eval_nif(Ref, Filename, Js, HandleRetval)),
            Pid ! Ret
        end
    ),
    receive
        Something -> Something
    after Timeout ->
        mozjs_nif:sm_cancel(Ref),
        {error, timeout}
    end.

sm_eval_nif(_Ref, _Filename, _Js, _HandleRetval) ->
    ?nif_stub.

sm_cancel(_Ref) ->
    ?nif_stub.

sm_stop(_Ref) ->
    ?nif_stub.
