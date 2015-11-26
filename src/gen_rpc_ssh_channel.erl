%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc_ssl_channel).

%%% Behaviour
-behaviour(ssh_daemon_channel).

%%% Include this library's name macro
-include("app.hrl").

%%% Local state
-record(state, {channelId :: non_neg_integer(),
        connectionMgr = undefined :: pid() | undefined,
        payload = <<>> :: iodata()}).

%%% ssh_daemon_channel callbacks
-export([subsystem_spec/1]).
-export([init/1, handle_ssh_msg/2, handle_msg/2, terminate/2, code_change/3]).

%%% ===================================================
%%% ssh_daemon_channel callbacks
%%% ===================================================

%% subsystem name conforms the private method and algorithm
%% convention on RFC4251 Section 6.
-spec subsystem_spec(term()) -> {string(), {term(), term()}}.
subsystem_spec(Options) -> {"gen_rpc", {?MODULE, Options}}.

init(_Args) ->
    ok = lager:info("function=init", []),
    process_flag(trap_exit, true),
    {ok,[]}.

%%--------------------------------------------------------------------
%% Function: handle_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles ssh channel messages.
%%--------------------------------------------------------------------
handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    ok = lager:debug("function=handle_msg event=ssh_channel_up channel_id=\"~p\" connection_mgr_pid=\"~p\"",
	                 [ChannelId, ConnectionManager]),
    {ok,  State#state{channelId = ChannelId, connectionMgr = ConnectionManager}};

handle_msg(Msg, #state{channelId = ChannelId} = State) ->
    ok = lager:critical("function=handle_msg event=unknown_msg_received message=\"~p\" action=stopping", [Msg]),
    {stop, ChannelId, State}.

%%--------------------------------------------------------------------
%% Function: handle_ssh_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles channel messages received on the ssh-connection.
%%--------------------------------------------------------------------
%% Type 0: RPC messages
%% other types: reserved
handle_ssh_msg({ssh_cm, ConnectionManager, {data, ChannelId, 0, Data}}, State) ->
    ok = lager:debug("function=handle_ssh_msg event=data channel_id=\"~p\" connection_mgr_pid=\"~p\"",
	                 [ChannelId, ConnectionManager]),
    State1 = handle_data(Data, State),
    {ok, State1};

handle_ssh_msg({ssh_cm, _, {eof, ChannelId}}, State) ->
    ok = lager:debug("function=handle_ssh_msg event=eof channel_id=\"~p\"", [ChannelId]),
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    ok = lager:debug("function=handle_ssh_msg event=signal", []),
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, Error, _}}, State) ->
    ok = lager:critical("function=handle_ssh_msg event=exit_signal channel_id=\"~p\" message=\"~p\"", [ChannelId, Error]),
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, 0}}, State) ->
    ok = lager:debug("function=handle_ssh_msg event=exit_status channel_id=\"~p\" status=0", [ChannelId]),
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, Status}}, State) ->
    ok = lager:critical("function=handle_ssh_msg event=exit_status channel_id=\"~p\" message=peer_close_connection status=\"~p\"", [ChannelId, Status]),
    {stop, ChannelId, State}.

terminate(Reason,_State) ->
    ok = lager:debug("function=terminate reason=\"~p\"", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ===================================================
%%% Private functions
%%% ===================================================
handle_data(<<Len:32/unsigned-big-integer, Msg:Len/binary, Rest/binary>>,
	    #state{connectionMgr = ConnectionManager, 
		   channelId = ChannelId,
		   payload = <<>>} = State) ->
    {Reply, Status} = exec_bin(Msg),
    Bin = term_to_binary({answer,{Reply, Status}}),
    Len = size(Bin),
    ssh_connection:send(ConnectionManager, ChannelId, 
			<<Len:32/unsigned-big-integer, Bin/binary>>),
    case Rest of
        <<>> -> State;
	_ -> handle_data(Rest, State)
    end;

handle_data(Data, State = #state{payload = <<>>}) ->
    State#state{payload = Data};

handle_data(Data, State = #state{payload = Pending}) ->
     handle_data(<<Pending/binary, Data/binary>>,
                 State#state{payload = <<>>}).

%% parsing a tuple and apply it

exec_bin(Cmdbin) -> 
    case binary_to_term(Cmdbin) of
	{mfa, {M, F, A}} -> 
	    case catch apply(M, F, A) of
		{'EXIT', _} = Exit ->
		    {{exec_badrpc, Exit}, -1};
		Reply ->
		    {Reply, 0}
	    end;
	Error -> 
	    {{exec_error,Error}, -1}
    end.

