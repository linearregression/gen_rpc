%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% Original concept inspired and some code copied from
%%% https://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles

-module(gen_rpc_ssl).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Include this library's name macro
-include("app.hrl").

-behaviour(gen_rpc_transport).

-export([name/0, close/1, controlling_process/2, connect/3, is_secure/0, listen/0, peername/1, send/2, setopts/2]).

-spec name() -> atom().
name() -> ssl.

-spec is_secure() -> boolean().
is_secure() -> true.

-spec connect(inet:ip_address(), port(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Address, Port, ConnTimeout) ->
	ssl:connect(Address, Port, gen_rpc_helper:default_tcp_opts(?DEFAULT_TCP_OPTS), ConnTimeout).

-spec close(ssl:sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

-spec controlling_process(inet:socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

-spec listen() -> {ok, inet:socket()} | {error, atom()}.
listen() ->
	ssl:listen(0, gen_rpc_helper:default_tcp_opts(?DEFAULT_TCP_OPTS)).

-spec send(inet:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
    ssl:send(Socket, Packet).

-spec setopts(inet:socket(), gen_tcp:option()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).  

-spec peername(inet:socket()) -> {ok, {inet:ip_address(), port()}} | {error, atom()}.
peername(Socket) ->
    inet:peername(Socket).
    



