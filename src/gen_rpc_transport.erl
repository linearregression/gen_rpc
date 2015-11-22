%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% Original concept inspired and some code copied from
%%% https://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles

-module(gen_rpc_transport).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

-callback name() -> atom().

-callback is_secure() -> boolean().

-callback connect(inet:ip_address(), inet:port_number(), timeout()) -> {ok, inet:socket()} | {error, atom()}.

-callback close(inet:socket()) -> ok.

-callback controlling_process(inet:socket(), pid()) -> ok | {error, closed | not_owner | atom()}.

-callback listen() -> {ok, inet:socket()} | {error, atom()}.

-callback send(inet:socket(), iodata()) -> ok | {error, atom()}.

-callback setopts(inet:socket(), gen_tcp:option()) -> ok | {error, atom()}.

-callback peername(inet:socket()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
    



