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

-export([name/0, accept/2, accept_ack/2, close/1, controlling_process/2, connect/3, is_secure/0, listen/0, listen/1, peername/1, port/1, recv/3, send/2, getopts/2, setopts/2, set_sockopt/2]).

-spec name() -> atom().
name() -> ssl.

-spec is_secure() -> boolean().
is_secure() -> true.

-spec accept(ssl:sslsocket(), timeout()) -> {ok, ssl:sslsocket()} | {error, closed | timeout | atom()}.
accept(Socket, Timeout) ->
	ssl:transport_accept(Socket, Timeout).

-spec accept_ack(ssl:sslsocket(), timeout()) -> {ok, ssl:sslsocket()} | {error, closed | timeout | atom()}.
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
	-> {ok, ssl:sslsocket()} | {error, atom()}.
connect(Address, Port, ConnTimeout) ->
	ssl:connect(Address, Port, gen_rpc_helper:default_tcp_opts(?DEFAULT_TCP_OPTS), ConnTimeout).

-spec close(ssl:sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

-spec controlling_process(ssl:sslsocket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

-spec listen() -> {ok, ssl:sslsocket()} | {error, atom()}.
listen() ->
	ssl:listen(0, gen_rpc_helper:default_tcp_opts(?DEFAULT_TCP_OPTS)).

-spec listen(inet:port_number()) -> {ok, ssl:sslsocket()} | {error, atom()}.
listen(Port) ->
	ssl:listen(Port, gen_rpc_helper:default_tcp_opts(?DEFAULT_TCP_OPTS)).

-spec recv(ssl:sslsocket(), non_neg_integer(), timeout()) -> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

-spec send(ssl:sslsocket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
    ssl:send(Socket, Packet).

-spec getopts(ssl:sslsocket(), ssl:option()) -> ssl:option() | {error, atom()}.
getopts(Socket, Opts) -> 
    ssl:getopts(Socket, Opts).

-spec setopts(ssl:sslsocket(), ssl:option()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
    ssl:setopts(Socket, Opts).  

-spec set_sockopt(inet:socket(), inet:socket()) -> ok | term().
set_sockopt(ListSock, AccSocket) -> 
    true = inet_db:register_socket(AccSocket, inet_tcp),
    case prim_inet:getopts(ListSock, ?DEFAULT_TCP_OPTS) of
        {ok, Opts} -> prim_inet:setopts(AccSocket, Opts), 
                      % Do an tcp to ssl upgrade 
                      upgrade_tcp(AccSocket) ;
        Error ->
            (catch gen_tcp:close(AccSocket)),
            Error
    end.

-spec upgrade_tcp(inet:socket()) -> ok | term().
upgrade_tcp(TcpSocket) ->
     case ssl:ssl_accept(TcpSocket, ?DEFAULT_TCP_OPTS) of
        {ok, AcceptSocket} -> 
             {ok, _Upgraded} = ssl:negotiated_protocol(AcceptSocket),
             ok;
        Error -> gen_tcp:close(TcpSocket), Error
    end.

-spec peername(ssl:sslsocket()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
    ssl:peername(Socket).
    
-spec port(ssl:sslsocket()) -> {ok, inet:port_number()} | {error, atom()}.
port(Socket) ->
    ssl_socket:port(Socket).



