%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(multi_rpc_functional_SUITE).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% CT Macros
-include_lib("test/gen_rpc/include/ct.hrl").

%%% Common Test callbacks
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%%% Testing functions
-export([supervisor_black_box/1,
        multi_call/1,
        multi_call_timeout/1,
        multi_call_mfa_undef/1,
        multi_call_mfa_exit/1,
        multi_call_mfa_throw/1,
        multi_call_inexistent_node/1,
        multi_call_no_node/1,
        multi_call_multiple_nodes/1]).

-export([start_slaves/0, stop_slaves/0]).

-define(TESTSRV, test_app_server).

%%% ===================================================
%%% CT callback functions
%%% ===================================================
all() ->
    {exports, Functions} = lists:keyfind(exports, 1, ?MODULE:module_info()),
    [FName || {FName, _} <- lists:filter(
                               fun ({module_info,_}) -> false;
                                   ({all,_}) -> false;
                                   ({init_per_suite,1}) -> false;
                                   ({end_per_suite,1}) -> false;

                                   ({_,1}) -> true;
                                   ({_,_}) -> false
                               end, Functions)].

init_per_suite(Config) ->
    %% Starting Distributed Erlang on local node
    {ok, _Pid} = gen_rpc_test_helper:start_target(?NODE),
    %% Setup application logging
    ?set_application_environment(),
    %% Starting the application locally
    {ok, _MasterApps} = application:ensure_all_started(?APP),
    ok = ct:pal("Started [functional] suite with master node [~s]", [node()]),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    ok = start_slaves(),
    Config.

end_per_testcase(_Test, Config) ->
    ok = stop_slaves(),
    Config.


%%% ===================================================
%%% Test cases
%%% ===================================================
%% Test supervisor's status
supervisor_black_box(_Config) ->
    ok = ct:pal("Testing [supervisor_black_box]"),
    true = erlang:is_process_alive(whereis(gen_rpc_server_sup)),
    true = erlang:is_process_alive(whereis(gen_rpc_acceptor_sup)),
    true = erlang:is_process_alive(whereis(gen_rpc_client_sup)),
    ok.

%% Test main functions
multi_call(_Config) ->
    ok = ct:pal("Testing [multi_call]"),
    [[?SLAVE1,?SLAVE2], []] = gen_rpc:multicall([?SLAVE1, ?SLAVE2], erlang, node, 5000, 100).

multi_call_timeout(_Config) ->
    ok = ct:pal("Testing [multi_call]"),
    [[], [?SLAVE1]] = gen_rpc:multicall([?SLAVE1], timer, sleep, [10000], 1, 1).

multi_call_mfa_undef(_Config) ->
    ok = ct:pal("Testing [multi_call_mfa_undef]"),
    [[], [?SLAVE1,?SLAVE2]] = gen_rpc:multicall([?SLAVE1, ?SLAVE2], erlang, undef, ['die'], 5000, 100).

multi_call_mfa_exit(_Config) ->
    ok = ct:pal("Testing [multi_call_mfa_exit]"),
    [[], [?SLAVE1,?SLAVE2]] = gen_rpc:multicall([?SLAVE1, ?SLAVE2], erlang, exit, ['die'], 5000, 100).

multi_call_mfa_throw(_Config) ->
    ok = ct:pal("Testing [multi_call_mfa_throw]"),

    [['throwXdown', 'throwXdown'], []] = gen_rpc:multicall([?SLAVE1, ?SLAVE2], erlang, throw, ['throwXdown'], 5000, 100).

multi_call_inexistent_node(_Config) ->
    ok = ct:pal("Testing [multi_call_inexistent_node]"),
    [[], [?FAKE_NODE]] = gen_rpc:multicall([?FAKE_NODE], os, timestamp, 10, 10).

multi_call_no_node(_Config) ->
    ok = ct:pal("Testing [multi_call_no_node]"),
    [[],[]] = gen_rpc:multicall([], os, timestamp, 10, 10).

multi_call_multiple_nodes(_Config) ->
    ok = ct:pal("Testing [multi_call_multiple_nodes]"),
    [[_,_],[?FAKE_NODE]] = gen_rpc:multicall([?SLAVE1, ?SLAVE2, ?FAKE_NODE], os, timestamp, 5000, 100).

%%% ===================================================
%%% Auxiliary functions for test cases
%%% ===================================================
start_slaves() ->
    ok = start_slave(?SLAVE_NAME1, ?SLAVE1),
    ok = start_slave(?SLAVE_NAME2, ?SLAVE2),
    ok = ct:pal("Slaves started"),
    ok.

start_slave(Name, Node) ->
    %% Starting a slave node with Distributed Erlang
    {ok, _Slave} = slave:start(?SLAVE_IP, Name, "+K true"),
    ok = rpc:call(Node, code, add_pathsz, [code:get_path()]),
    %% Start the application remotely
    {ok, _SlaveApps} = rpc:call(Node, application, ensure_all_started, [gen_rpc]),
    {module, gen_rpc_test_helper} = rpc:call(Node, code, ensure_loaded, [gen_rpc_test_helper]),
    ok.

stop_slaves() ->
    Slaves = [?SLAVE1, ?SLAVE2],
    [begin 
        ok = slave:stop(Node)
     end || Node <- Slaves],
    ok = ct:pal("Slaves stopped").
