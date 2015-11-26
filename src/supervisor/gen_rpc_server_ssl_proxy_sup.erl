%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc_server_ssl_proxy_sup).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Behaviour
-behaviour(supervisor).

%%% Include this library's name macro
-include("app.hrl").

%%% gen_rpc_server_ssl_proxy_sup functions
-export([start_link/0]).

%%% Supervisor callbacks
-export([init/1]).

%%% ===================================================
%%% Supervisor functions
%%% ===================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% ===================================================
%%% Supervisor callbacks
%%% ===================================================
init([]) ->
    SystemDir = application:get_env(?APP,system_dir,"/etc/ssh"),
    Port = application:get_env(?APP, port, 443),
    SshOpts = [
	       {system_dir,SystemDir},
	       {key_cb,key_handler},
	       {subsystems,[{"gen_rpc_ssl_channel",{gen_rpc_ssl_channel,[]}}]},
	       {ssh_cli,no_cli}
	      ],
    {ok, {{simple_one_for_one, 100, 1}, [{daemon,{ssh,daemon,[Port,SshOpts]},permanent,5000,worker,[ssh]}]}}.



