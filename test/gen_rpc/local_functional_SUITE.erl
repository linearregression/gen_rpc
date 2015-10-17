%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(local_functional_SUITE).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% CT Macros
-include_lib("test/gen_rpc/include/ct.hrl").

%%% Common Test callbacks
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%%% Testing functions
-export([supervisor_black_box/1,
        call/1,
        call_anonymous_function/1,
        call_anonymous_undef/1,
        call_mfa_undef/1,
        call_mfa_exit/1,
        call_mfa_throw/1,
        call_with_receive_timeout/1,
        interleaved_call/1,
        cast/1,
        cast_anonymous_function/1,
        cast_mfa_undef/1,
        cast_mfa_exit/1,
        cast_mfa_throw/1,
        cast_inexistent_node/1,
        safe_cast/1,
        safe_cast_anonymous_function/1,
        safe_cast_mfa_undef/1,
        safe_cast_mfa_exit/1,
        safe_cast_mfa_throw/1,
        safe_cast_inexistent_node/1,
        async_call/1,
        async_call_anonymous_function/1,
        async_call_anonymous_undef/1,
        async_call_mfa_undef/1,
        async_call_mfa_exit/1,
        async_call_mfa_throw/1,
        async_call_yield_timeout/1,
        async_call_yield_infinity/1,
        client_inactivity_timeout/1,
        server_inactivity_timeout/1,
        remote_node_call/1]).

%%% Auxiliary functions for test cases
-export([interleaved_call_proc/3, interleaved_call_executor/1]).

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
                                   ({interleaved_call_proc,3}) -> false;
                                   ({interleaved_call_executor,1}) -> false;
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

init_per_testcase(client_inactivity_timeout, Config) ->
    ok = ?restart_application(),
    ok = application:set_env(?APP, client_inactivity_timeout, 500),
    Config;
init_per_testcase(server_inactivity_timeout, Config) ->
    ok = ?restart_application(),
    ok = application:set_env(?APP, server_inactivity_timeout, 500),
    Config;
init_per_testcase(remote_node_call, Config) ->
    ok = start_slave(),
    Config;
init_per_testcase(async_call_yield_timeout, Config) ->
    ok = start_slave(),
    ok = application:set_env(?APP, yield_timeout, 10),
    Config;
init_per_testcase(_OtherTest, Config) ->
    Config.
end_per_testcase(client_inactivity_timeout, Config) ->
    ok = ?restart_application(),
    ok = application:set_env(?APP, client_inactivity_timeout, infinity),
    Config;
end_per_testcase(server_inactivity_timeout, Config) ->
    ok = ?restart_application(),
    ok = application:set_env(?APP, server_inactivity_timeout, infinity),
    Config;
end_per_testcase(remote_node_call, Config) ->
    ok = slave:stop(?SLAVE),
    Config;
end_per_testcase(async_call_yield_timeout, Config) ->
    ok = slave:stop(?SLAVE),
    ok = application:set_env(?APP, yield_timeout, infinity),
    Config;
end_per_testcase(_OtherTest, Config) ->
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
call(_Config) ->
    ok = ct:pal("Testing [call]"),
    {_Mega, _Sec, _Micro} = gen_rpc:call(?NODE, os, timestamp).

call_anonymous_function(_Config) ->
    ok = ct:pal("Testing [call_anonymous_function]"),
    {_,"\"call_anonymous_function\""} = gen_rpc:call(?NODE, erlang, apply,[fun(A) -> {self(), io_lib:print(A)} end,
                                                     ["call_anonymous_function"]]).

call_anonymous_undef(_Config) ->
    ok = ct:pal("Testing [call_anonymous_undef]"),
    ok = ct:pal("Testing [call_anonymous_undef] Assumping stackstack depth is 5"),
    {badrpc, {'EXIT', {undef,[{os,timestamp_undef,[],[]},_]}}}  = gen_rpc:call(?NODE, erlang, apply, [fun() -> os:timestamp_undef() end, []]),
   ok = ct:pal("Result [call_anonymous_undef]: signal=EXIT Reason={os,timestamp_undef}").

call_mfa_undef(_Config) ->
    ok = ct:pal("Testing [call_mfa_undef]"),
    {badrpc, {'EXIT', {undef,[{os,timestamp_undef,_,_},_]}}} = gen_rpc:call(?NODE, os, timestamp_undef),
    ok = ct:pal("Result [call_mfa_undef]: signal=EXIT Reason={os,timestamp_undef}").

call_mfa_exit(_Config) ->
    ok = ct:pal("Testing [call_mfa_exit]"),
    {badrpc, {'EXIT', die}} = gen_rpc:call(?NODE, erlang, exit, ['die']),
    ok = ct:pal("Result [call_mfa_undef]: signal=EXIT Reason={die}").

call_mfa_throw(_Config) ->
    ok = ct:pal("Testing [call_mfa_throw]"),
    'throwXdown' = gen_rpc:call(?NODE, erlang, throw, ['throwXdown']),
    ok = ct:pal("Result [call_mfa_undef]: signal=EXIT Reason={throwXdown}").

call_with_receive_timeout(_Config) ->
    ok = ct:pal("Testing [call_with_receive_timeout]"),
    {badrpc, timeout} = gen_rpc:call(?NODE, timer, sleep, [500], 1),
    ok = timer:sleep(500).

interleaved_call(_Config) ->
    ok = ct:pal("Testing [interleaved_call]"),
    %% Spawn 3 consecutive processes that execute gen_rpc:call
    %% to the remote node and wait an inversely proportionate time
    %% for their result (effectively rendering the results out of order)
    %% in order to test proper data interleaving
    Pid1 = erlang:spawn(?MODULE, interleaved_call_proc, [self(), 1, infinity]),
    Pid2 = erlang:spawn(?MODULE, interleaved_call_proc, [self(), 2, 10]),
    Pid3 = erlang:spawn(?MODULE, interleaved_call_proc, [self(), 3, infinity]),
    ok = interleaved_call_loop(Pid1, Pid2, Pid3, 0),
    ok.

cast(_Config) ->
    ok = ct:pal("Testing [cast]"),
    true = gen_rpc:cast(?NODE, erlang, timestamp).

cast_anonymous_function(_Config) ->
    ok = ct:pal("Testing [cast_anonymous_function]"),
    true = gen_rpc:cast(?NODE, erlang, apply, [fun() -> os:timestamp() end, []]).

cast_mfa_undef(_Config) ->
    ok = ct:pal("Testing [cast_mfa_undef]"),
    true = gen_rpc:cast(?NODE, os, timestamp_undef, []).

cast_mfa_exit(_Config) ->
    ok = ct:pal("Testing [cast_mfa_exit]"),
    true = gen_rpc:cast(?NODE, erlang, apply, [fun() -> exit(die) end, []]).

cast_mfa_throw(_Config) ->
    ok = ct:pal("Testing [cast_mfa_throw]"),
    true = gen_rpc:cast(?NODE, erlang, throw, ['throwme']).

cast_inexistent_node(_Config) ->
    ok = ct:pal("Testing [cast_inexistent_node]"),
    true = gen_rpc:cast(?FAKE_NODE, os, timestamp, [], 1000).

safe_cast(_Config) ->
    ok = ct:pal("Testing [safe_cast]"),
    true = gen_rpc:safe_cast(?NODE, erlang, timestamp).

safe_cast_anonymous_function(_Config) ->
    ok = ct:pal("Testing [safe_cast_anonymous_function]"),
    true = gen_rpc:safe_cast(?NODE, erlang, apply, [fun() -> os:timestamp() end, []]).

safe_cast_mfa_undef(_Config) ->
    ok = ct:pal("Testing [safe_cast_mfa_undef]"),
    true = gen_rpc:safe_cast(?NODE, os, timestamp_undef, []).

safe_cast_mfa_exit(_Config) ->
    ok = ct:pal("Testing [safe_cast_mfa_exit]"),
    true = gen_rpc:safe_cast(?NODE, erlang, apply, [fun() -> exit(die) end, []]).

safe_cast_mfa_throw(_Config) ->
    ok = ct:pal("Testing [safe_cast_mfa_throw]"),
    true = gen_rpc:safe_cast(?NODE, erlang, throw, ['throwme']).

safe_cast_inexistent_node(_Config) ->
    ok = ct:pal("Testing [safe_cast_inexistent_node]"),
    {badrpc, nodedown} = gen_rpc:safe_cast(?FAKE_NODE, os, timestamp, [], 1000).

async_call(_Config) ->
    ok = ct:pal("Testing [async_call]"),
    YieldKey0 = gen_rpc:async_call(?NODE, erlang, timestamp, []),
    {_Mega, _Sec, _Micro} = gen_rpc:yield(YieldKey0),
    NbYieldKey0 = gen_rpc:async_call(?NODE, erlang, timestamp, []),
    {value,{_,_,_}}= gen_rpc:nb_yield(NbYieldKey0, 10),
    YieldKey = gen_rpc:async_call(?NODE, io_lib, print, [yield_key]),
    "yield_key" = gen_rpc:yield(YieldKey),
    NbYieldKey = gen_rpc:async_call(?NODE, io_lib, print, [nb_yield_key]),
    {value, "nb_yield_key"} = gen_rpc:nb_yield(NbYieldKey, 10).

async_call_anonymous_function(_Config) ->
    ok = ct:pal("Testing [async_call_anonymous_function]"),
    YieldKey = gen_rpc:async_call(?NODE, erlang, apply,[fun(A) -> {self(), io_lib:print(A)} end,
                                                     [yield_key_anonymous_func]]),
    {_, "yield_key_anonymous_func"} = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?NODE, erlang, apply,[fun(A) -> {self(), io_lib:print(A)} end,
                                                     [nb_yield_key_anonymous_func]]),
    {value, {_, "nb_yield_key_anonymous_func"}} = gen_rpc:nb_yield(NBYieldKey, 10).

async_call_anonymous_undef(_Config) ->
    ok = ct:pal("Testing [async_call_anonymous_undef]"),
    YieldKey = gen_rpc:async_call(?NODE, erlang, apply, [fun() -> os:timestamp_undef() end, []]),
    {badrpc, {'EXIT', {undef,[{os,timestamp_undef,[],[]},_]}}} = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?NODE, erlang, apply, [fun() -> os:timestamp_undef() end, []]),
    {value, {badrpc, {'EXIT', {undef,[{os,timestamp_undef,[],[]},_]}}}} = gen_rpc:nb_yield(NBYieldKey, 10),
    ok = ct:pal("Result [async_call_anonymous_undef]: signal=EXIT Reason={os,timestamp_undef}").

async_call_mfa_undef(_Config) ->
    ok = ct:pal("Testing [async_call_mfa_undef]"),
    YieldKey = gen_rpc:async_call(?NODE, os, timestamp_undef),
    {badrpc, {'EXIT', {undef,[{os,timestamp_undef,_,_},_]}}} = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?NODE, erlang, apply, [fun() -> os:timestamp_undef() end, []]),
    {value, {badrpc, {'EXIT', {undef,[{os,timestamp_undef,_,_},_]}}}} = gen_rpc:nb_yield(NBYieldKey, 20),
    ok = ct:pal("Result [async_call_mfa_undef]: signal=EXIT Reason={os,timestamp_undef}").

async_call_mfa_exit(_Config) ->
    ok = ct:pal("Testing [async_call_mfa_exit]"),
    YieldKey = gen_rpc:async_call(?NODE, erlang, exit, ['die']),
    {badrpc, {'EXIT', die}} = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?NODE, erlang, exit, ['die']),
    {value, {badrpc, {'EXIT', die}}} = gen_rpc:nb_yield(NBYieldKey, 10),
    ok = ct:pal("Result [async_call_mfa_undef]: signal=EXIT Reason={os,timestamp_undef}").

async_call_mfa_throw(_Config) ->
    ok = ct:pal("Testing [async_call_mfa_throw]"),
    YieldKey = gen_rpc:async_call(?NODE, erlang, throw, ['throwXdown']),
    'throwXdown' = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?NODE, erlang, throw, ['throwXdown']),
    {value, 'throwXdown'} = gen_rpc:nb_yield(NBYieldKey, 10),
    ok = ct:pal("Result [async_call_mfa_undef]: signal=EXIT Reason={throwXdown}").

async_call_yield_timeout(_Config) ->
    ok = ct:pal("Testing [async_call_yield_timeout]"),
    YieldKey = gen_rpc:async_call(?NODE, timer, sleep, [1000]),
    {badrpc,timeout} = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?NODE, timer, sleep, [1000]),
    {badrpc,timeout} = gen_rpc:nb_yield(NBYieldKey, 5),
    ok = ct:pal("Result [async_call_yield_timeout]: signal=EXIT Reason={timeout}").

async_call_yield_infinity(_Config) ->
    ok = ct:pal("Testing [async_call_yield_infinity]"),
    YieldKey = gen_rpc:async_call(?NODE, timer, sleep, [1000]),
    ok = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?NODE, timer, sleep, [1000]),
    {value, ok} = gen_rpc:nb_yield(NBYieldKey, infinity),
    ok = ct:pal("Result [async_call_yield_infinity]: signal=EXIT Reason={ok}").


client_inactivity_timeout(_Config) ->
    ok = ct:pal("Testing [client_inactivity_timeout]"),
    {_Mega, _Sec, _Micro} = gen_rpc:call(?NODE, os, timestamp),
    ok = timer:sleep(600),
    %% Lookup the client named process, shouldn't be there
    undefined = whereis(?NODE).

server_inactivity_timeout(_Config) ->
    ok = ct:pal("Testing [server_inactivity_timeout]"),
    {_Mega, _Sec, _Micro} = gen_rpc:call(?NODE, os, timestamp),
    ok = timer:sleep(600),
    %% Lookup the client named process, shouldn't be there
    [] = supervisor:which_children(gen_rpc_acceptor_sup),
    %% The server supervisor should have no children
    [] = supervisor:which_children(gen_rpc_server_sup).

remote_node_call(_Config) ->
    ok = ct:pal("Testing [remote_node_call]"),
    {_Mega, _Sec, _Micro} = gen_rpc:call(?SLAVE, os, timestamp).

%%% ===================================================
%%% Auxiliary functions for test cases
%%% ===================================================
%% Loop in order to receive all messages from all workers
interleaved_call_loop(Pid1, Pid2, Pid3, Num) when Num < 3 ->
    receive
        {reply, Pid1, 1, 1} ->
            ct:pal("Received proper reply from Worker 1"),
            interleaved_call_loop(Pid1, Pid2, Pid3, Num+1);
        {reply, Pid2, 2, {badrpc, timeout}} ->
            ct:pal("Received proper reply from Worker 2"),
            interleaved_call_loop(Pid1, Pid2, Pid3, Num+1);
        {reply, Pid3, 3, 3} ->
            ct:pal("Received proper reply from Worker 3"),
            interleaved_call_loop(Pid1, Pid2, Pid3, Num+1);
        _Else ->
            ct:pal("Received out of order reply"),
            fail
    end;
interleaved_call_loop(_, _, _, 3) ->
    ok.

%% This function will become a spawned process that performs the RPC
%% call and then returns the value of the RPC call
%% We spawn it in order to achieve parallelism and test out-of-order
%% execution of multiple RPC calls
interleaved_call_proc(Caller, Num, Timeout) ->
    Result = gen_rpc:call(?NODE, ?MODULE, interleaved_call_executor, [Num], Timeout),
    Caller ! {reply, self(), Num, Result},
    ok.

%% This is the function that gets executed in the "remote"
%% node, sleeping 3 minus $Num seconds and returning the number
%% effectively returning a number thats inversely proportional
%% to the number of seconds the worker slept
interleaved_call_executor(Num) when is_integer(Num) ->
    %% Sleep for 3 - Num
    ok = timer:sleep((3 - Num) * 1000),
    %% Then return the number
    Num.

start_slave() ->
    %% Starting a slave node with Distributed Erlang
    {ok, _Slave} = slave:start(?SLAVE_IP, ?SLAVE_NAME, "+K true"),
    ok = rpc:call(?SLAVE, code, add_pathsz, [code:get_path()]),
    %% Start the application remotely
    {ok, _SlaveApps} = rpc:call(?SLAVE, application, ensure_all_started, [gen_rpc]),
    ok.

