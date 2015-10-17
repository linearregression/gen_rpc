%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Library interface
-export([async_call/3,
        async_call/4,
        call/3,
        call/4,
        call/5,
        call/6,
        cast/3,
        cast/4,
        cast/5,
        yield/1,
        nb_yield/1,
        nb_yield/2,
        safe_cast/3,
        safe_cast/4,
        safe_cast/5]).

%%% ===================================================
%%% Library interface
%%% ===================================================
%% All functions are GUARD-ed in the sender module, no
%% need for the overhead here
-spec async_call(Node::node(), M::module(), F::atom()|function()) -> term() | {'badrpc', term()} | {'badtcp' | term()}.
async_call(Node, M, F) ->
    gen_rpc_client:async_call(Node, M, F).

-spec async_call(Node::node(), M::module(), F::atom()|function(), A::list()) -> term() | {'badrpc', term()} | {'badtcp' | term()}.
async_call(Node, M, F, A) ->
    gen_rpc_client:async_call(Node, M, F, A).

-spec call(Node::node(), M::module(), F::atom()|function()) -> term() | {'badrpc', term()} | {'badtcp' | term()}.
call(Node, M, F) ->
    gen_rpc_client:call(Node, M, F).

-spec call(Node::node(), M::module(), F::atom()|function(), A::list()) -> term() | {'badrpc', term()} | {'badtcp' | term()}.
call(Node, M, F, A) ->
    gen_rpc_client:call(Node, M, F, A).

-spec call(Node::node(), M::module(), F::atom()|function(), A::list(), RecvTO::timeout()) -> term() | {'badrpc', term()} | {'badtcp' | term()}.
call(Node, M, F, A, RecvTO) ->
    gen_rpc_client:call(Node, M, F, A, RecvTO).

-spec call(Node::node(), M::module(), F::atom()|function(), A::list(), RecvTO::timeout(), SendTO::timeout()) -> term() | {'badrpc', term()} | {'badtcp' | term()}.
call(Node, M, F, A, RecvTO, SendTO) ->
    gen_rpc_client:call(Node, M, F, A, RecvTO, SendTO).

-spec cast(Node::node(), M::module(), F::atom()|function()) -> 'true'.
cast(Node, M, F) ->
    gen_rpc_client:cast(Node, M, F).

-spec cast(Node::node(), M::module(), F::atom()|function(), A::list()) -> 'true'.
cast(Node, M, F, A) ->
    gen_rpc_client:cast(Node, M, F, A).

-spec cast(Node::node(), M::module(), F::atom()|function(), A::list(), SendTO::timeout()) -> 'true'.
cast(Node, M, F, A, SendTO) ->
    gen_rpc_client:cast(Node, M, F, A, SendTO).

-spec safe_cast(Node::node(), M::module(), F::atom()|function()) -> 'true' | {'badrpc', term()} | {'badtcp' | term()}.
safe_cast(Node, M, F) ->
    gen_rpc_client:safe_cast(Node, M, F).

-spec safe_cast(Node::node(), M::module(), F::atom()|function(), A::list()) -> 'true' | {'badrpc', term()} | {'badtcp' | term()}.
safe_cast(Node, M, F, A) ->
    gen_rpc_client:safe_cast(Node, M, F, A).

-spec safe_cast(Node::node(), M::module(), F::atom()|function(), A::list(), SendTO::timeout()) -> 'true' | {'badrpc', term()} | {'badtcp' | term()}.
safe_cast(Node, M, F, A, SendTO) ->
    gen_rpc_client:safe_cast(Node, M, F, A, SendTO).

-spec yield(Key::pid()) -> term() | {badrpc, term()}.
yield(Key) ->
    gen_rpc_client:yield(Key).

-spec nb_yield(Key::pid()) -> {value, term()} | {badrpc, term()}.
nb_yield(Key) ->
    gen_rpc_client:nb_yield(Key).

-spec nb_yield(Key::pid(), Timeout::timeout()) -> {value, term()} | {badrpc, term()}.
nb_yield(Key, Timeout) ->
    gen_rpc_client:nb_yield(Key, Timeout).

