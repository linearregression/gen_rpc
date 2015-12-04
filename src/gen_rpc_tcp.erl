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

-define(ACCEPTOR_DEFAULT_TCP_OPTS, [binary, {packet,4},
        {active,once}]).

%%% The TCP options that should be copied from the listener to the acceptor
-define(DEFAULT_TCP_LISTEN_OPTS, [nodelay,
        send_timeout_close,
        delay_send,
        linger,
        reuseaddr,
        keepalive,
        tos,
        active]).

-behaviour(gen_rpc_transport).

-export([name/0, accept/2, accept_ack/2, close/1, controlling_process/2, connect/3,
         is_secure/0, listen/0, listen/1, peername/1, port/1, recv/3, send/2, getopts/2,
         setopts/2, set_sockopt/2]).

-spec name() -> atom().
name() -> tcp.

-spec is_secure() -> boolean().
is_secure() -> false.

-spec accept(inet:socket(), timeout()) -> {ok, inet:socket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	prim_inet:async_accept(LSocket, Timeout).

-spec accept_ack(inet:socket(), timeout()) -> {ok, inet:socket()} | {error, closed | timeout | atom()}.
accept_ack(Socket, Timeout) ->
	case ssl:ssl_accept(Socket, Timeout) of
		ok -> ok;
		%% Garbage was most likely sent to the socket, don't error out.
		{error, {tls_alert, _}} -> 
			ok = close(Socket),
			exit(normal);
		%% Socket most likely stopped responding, don't error out.
		{error, Reason} when Reason =:= timeout; Reason =:= closed ->
			ok = close(Socket),
			exit(normal);
		{error, Reason} ->
			ok = close(Socket),
			error(Reason)
	end.

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
listen() -> listen(0). 

-spec listen(inet:port_number()) -> {ok, inet:socket()} | {error, atom()}.
listen(Port) ->
	gen_tcp:listen(Port, gen_rpc_helper:default_tcp_opts(?DEFAULT_TCP_OPTS)).

-spec recv(inet:socket(), non_neg_integer(), timeout()) -> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	gen_tcp:recv(Socket, Length, Timeout).

-spec send(inet:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
    gen_tcp:send(Socket, Packet).

-spec getopts(inet:socket(), gen_tcp:option()) -> {ok, gen_tcp:option()} | {error, atom()}.
getopts(Socket, Opts) ->
    prim_inet:getopts(Socket, Opts).

-spec setopts(inet:socket(), gen_tcp:option()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).  

-spec set_sockopt(inet:socket(), inet:socket()) -> ok | term().
set_sockopt(ListSock, AccSocket) -> 
    true = inet_db:register_socket(AccSocket, inet_tcp),
    case prim_inet:getopts(ListSock, ?DEFAULT_TCP_LISTEN_OPTS) of
        {ok, Opts} ->
            case prim_inet:setopts(AccSocket, Opts) of
                ok    -> ok;
                Error -> gen_tcp:close(AccSocket), Error
            end;
        Error ->
            (catch gen_tcp:close(AccSocket)),
            Error
    end.

-spec peername(inet:socket()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
    inet:peername(Socket).

-spec port(inet:socket()) -> {ok, inet:port_number()} | {error, atom()}.
port(Socket) ->
    ssl_socket:port(Socket).    




