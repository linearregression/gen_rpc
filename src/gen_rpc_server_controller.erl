%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% Original concept inspired and some code copied from
%%% https://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles

-module(gen_rpc_server_controller).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Behaviour
-behaviour(gen_server).

%%% Include this library's name macro
-include("app.hrl").

%%% Local state
-record(state, {transport :: module(),
        socket :: port(),
        acceptor_pid :: pid(),
        acceptor :: non_neg_integer()}).

%%% Default TCP options
-define(ACCEPTOR_DEFAULT_TCP_OPTS, [binary, {packet,4},
        {active,once}]). % Retrieve data from socket upon request

%%% Supervisor functions
-export([start_link/1, stop/1]).

%%% API
-export([connect/1, call/1]).

%%% Behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, terminate/2, code_change/3]).

%%% ===================================================
%%% Supervisor functions
%%% ===================================================
start_link(Node) ->
    Name = make_process_name(Node),
    gen_server:start_link({local,Name}, ?MODULE, [], [{spawn_opt, [{priority, high}]}]).

stop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, stop).

%%% ===================================================
%%% Behaviour callbacks
%%% ===================================================
init([]) ->
    ok = lager:info("function=init", []),
    process_flag(trap_exit, true),  
    {ok, Settings} = get_transport_settings(),
    {ok, Module} = get_transport_module(Settings),
    {ok, Port} = get_listen_port(Settings), 
    case listen(Port, Module) of
        {ok, Socket} ->
            ok = lager:info("function=init event=listener_started_successfully", []),
%            {ok, _Ref} = prim_inet:async_accept(Socket, -1),
            {ok, _Ref} = Module:accept(Socket, -1),
            {ok, #state{transport = Module,
                        socket = Socket}};
        {error, Reason} ->
            ok = lager:critical("function=init event=failed_to_start_listener reason=\"~p\"", [Reason]),
            {stop, Reason}
    end.

%% Listen for incoming request to start_child
-spec listen(port(), module()) -> {ok, pid()} | {error, term()}.
listen(Port, Module) ->
    case Module:listen(Port, gen_rpc_helper:default_tcp_opts(Module)) of
        {ok, Socket} ->
            ok = lager:info("function=init event=listener_started_successfully", []),
            {ok, Socket}; 
        {error, Reason} ->
            ok = lager:critical("function=init event=failed_to_start_listener reason=\"~p\"", [Reason]),
            {stop, Reason}
    end.

%% Gracefully stop
handle_call(stop, _From, State) ->
    ok = lager:debug("function=handle_call message=stop event=stopping_server socket=\"~p\"", [State#state.socket]),
    {stop, normal, ok, State};

%% Catch-all for calls - die if we get a message we don't expect
handle_call(Msg, _From, State) ->
    ok = lager:critical("function=handle_call event=unknown_call_received socket=\"~p\" message=\"~p\" action=stopping", [State#state.socket, Msg]),
    {stop, {unknown_call, Msg}, {unknown_call, Msg}, State}.

%% Catch-all for casts - die if we get a message we don't expect
handle_cast(Msg, State) ->
    ok = lager:critical("function=handle_cast event=unknown_cast_received socket=\"~p\" message=\"~p\" action=stopping", [State#state.socket, Msg]),
    {stop, {unknown_cast, Msg}, State}.

%% Catch-all for info - our protocol is strict so die!
handle_info(Msg, State) ->
    ok = lager:critical("function=handle_info event=unknown_message_received socket=\"~p\" message=\"~p\" action=stopping", [State#state.socket, Msg]),
    {stop, {unknown_message, Msg}, State}.

%% Terminate cleanly by closing the listening socket
terminate(_Reason, #state{transport=Module,socket=Socket}) ->
    ok = lager:debug("function=terminate socket=\"~p\"", [Socket]),
    % Politey signal client to close socket
    catch Module:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ===================================================
%%% Private functions
%%% ===================================================
get_transport_settings() ->
    application:get_env(?APP, 'transport_module').

get_transport_module(Settings) ->
    {module, Module} = lists:keyfind(module, 1, Settings),
    {file, _} = code:is_loaded(Module),
    {ok, Module}.

get_listen_port(Settings) ->
    {'control_port', ControlPort} = lists:keyfind('control_port', 1, Settings),
    {ok, ControlPort}. 

make_process_name(Node) ->
    NodeBin = atom_to_binary(Node, latin1),
    binary_to_atom(<<"gen_rpc_server_controller_", NodeBin/binary>>, latin1).

