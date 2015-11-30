%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc_helper).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

-include("app.hrl").

-export([otp_release/0, default_socket_opts/1, default_tcp_opts/1, verify_transport_mode/1]).

-spec otp_release() -> integer().
otp_release() ->
    try
        erlang:list_to_integer(erlang:system_info(otp_release))
    catch
        error:badarg ->
            %% Before Erlang 17, R was included in the OTP release,
            %% which would make the list_to_integer call fail.
            %% Since we only use this function to test the availability
            %% of the show_econnreset feature, 16 is good enough.
            16
    end.

-spec default_socket_opts(module()) -> gen_tcp:option() | ssl:option().
default_socket_opts('gen_rpc_tcp') -> default_tcp_opts(?DEFAULT_TCP_OPTS);
default_socket_opts('gen_rpc_ssl') -> throw('not_implemented').

-spec default_tcp_opts(gen_tcp:option()) ->  gen_tcp:option().
default_tcp_opts(DefaultTcpOpts) ->
    case otp_release() >= 18 of
        true ->
            [{show_econnreset, true}|DefaultTcpOpts];
        false ->
            DefaultTcpOpts
    end.

-spec verify_transport_mode(gen_tcp|ssl) -> ok | {unsupported, term()}.
verify_transport_mode(gen_rpc_tcp) -> {ok, 'gen_rpc_tcp'};
verify_transport_mode(gen_rpc_ssl) -> {ok, 'gen_rpc_ssl'};
verify_transport_mode(Else) -> {unspported, Else}.
