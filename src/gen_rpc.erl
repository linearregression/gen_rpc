%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Behaviour
-behaviour(application).

%%% Include this library's name macro
-include("app.hrl").

%%% Application callbacks
-export([start/2, stop/1]).

%%% Development start/stop functions
-export([start/0, start/1, stop/0]).

%%% Library interface
-export([call/3, call/4, call/5, call/6, cast/3, cast/4, cast/5]).

%%% ===================================================
%%% Application callbacks
%%% ===================================================
start(_StartType, _StartArgs) ->
    gen_rpc_sup:start_link().

stop(_State) ->
    ok.

%%% ===================================================
%%% Application callbacks
%%% ===================================================
start() ->
    start([]).

start(Options) ->
    [application:set_env(?APP, Key, Value ) || {Key, Value} <- Options],
    application:start(?APP).

stop() ->
    application:stop(?APP).

%%% ===================================================
%%% Library interface
%%% ===================================================
%% All functions are GUARD-ed in the sender module, no
%% need for the overhead here
call(Node, M, F) ->
    gen_rpc_client:call(Node, M, F).

call(Node, M, F, A) ->
    gen_rpc_client:call(Node, M, F, A).

call(Node, M, F, A, RecvTO) ->
    gen_rpc_client:call(Node, M, F, A, RecvTO).

call(Node, M, F, A, RecvTO, SendTO) ->
    gen_rpc_client:call(Node, M, F, A, RecvTO, SendTO).

cast(Node, M, F) ->
    gen_rpc_client:cast(Node, M, F).

cast(Node, M, F, A) ->
    gen_rpc_client:cast(Node, M, F, A).

cast(Node, M, F, A, SendTO) ->
    gen_rpc_client:cast(Node, M, F, A, SendTO).
