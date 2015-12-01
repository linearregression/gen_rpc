%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% Original concept inspired and some code copied from
%%% https://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles

-module(gen_rpc_tcp).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Include this library's name macro
-include("app.hrl").

-behaviour(gen_rpc_transport).

-export([name/0, accept/2, close/1, controlling_process/2, connect/3, is_secure/0, listen/0, peername/1, send/2, getopts/2, setopts/2]).

-spec name() -> atom().
name() -> tcp.

-spec is_secure() -> boolean().
is_secure() -> false.

-spec accept(inet:socket(), timeout()) -> {ok, inet:socket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	prim_inet:async_accept(LSocket, Timeout).
%	gen_tcp:accept(LSocket, Timeout).

-spec connect(inet:ip_address(), inet:port_number(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Address, Port, ConnTimeout) ->
	gen_tcp:connect(Address, Port, gen_rpc_helper:default_tcp_opts(?DEFAULT_TCP_OPTS), ConnTimeout).

-spec close(inet:socket()) -> ok.
close(Socket) ->
	gen_tcp:close(Socket).

-spec controlling_process(inet:socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	gen_tcp:controlling_process(Socket, Pid).

-spec listen() -> {ok, inet:socket()} | {error, atom()}.
listen() ->
	gen_tcp:listen(0, gen_rpc_helper:default_tcp_opts(?DEFAULT_TCP_OPTS)).

-spec send(inet:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
    gen_tcp:send(Socket, Packet).

-spec getopts(inet:socket(), gen_tcp:option()) -> {ok, gen_tcp:option()} | {error, atom()}.
getopts(Socket, Opts) ->
    prim_inet:getopts(Socket, Opts).

-spec setopts(inet:socket(), gen_tcp:option()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).  

-spec peername(inet:socket()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
    inet:peername(Socket).
    




