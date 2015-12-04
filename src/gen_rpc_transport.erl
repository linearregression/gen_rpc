%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% Original concept inspired and some code copied from
%%% https://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles

-module(gen_rpc_transport).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").
-type socket() :: inet:socket() | ssl:socket().
-type opts() :: gen_tcp:option() | ssl:option().

-callback name() -> atom().
-callback is_secure() -> boolean().
-callback connect(inet:ip_address(), inet:port_number(), timeout()) -> {ok, inet:socket()} | {error, atom()}.
-callback close(socket()) -> ok.
-callback controlling_process(socket(), pid()) -> ok | {error, closed | not_owner | atom()}.
-callback listen() -> {ok, socket()} | {error, atom()}.
-callback listen(port()) -> {ok, socket()} | {error, atom()}.
-callback accept(socket(), timeout()) -> {ok, socket()} | {error, closed | timeout | atom()}.
-callback accept_ack(socket(), timeout()) -> ok.
-callback recv(socket(), non_neg_integer(), timeout()) -> {ok, any()} | {error, closed | timeout | atom()}.
-callback send(socket(), iodata()) -> ok | {error, atom()}.
-callback getopts(socket(), opts()) -> opts() | {error, atom()}.
-callback setopts(socket(), opts()) -> ok | {error, atom()}.
-callback peername(socket()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
-callback port(socket()) -> inet:port_number().
-callback set_sockopt(socket(), socket()) -> ok | term().
    




