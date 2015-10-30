%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

%%% Common Test includes
-include_lib("common_test/include/ct.hrl").
%%% Include this library's name macro
-include_lib("gen_rpc/include/app.hrl").

%%% Node definitions
-define(NODE, 'gen_rpc_master@127.0.0.1').
-define(SLAVE, 'gen_rpc_slave@127.0.0.1').
-define(SLAVE1, 'gen_rpc_slave1@127.0.0.1').
-define(SLAVE2, 'gen_rpc_slave2@127.0.0.1').
-define(FAKE_NODE, 'fake_node@127.0.1.1').
-define(SLAVE_IP, '127.0.0.1').
-define(SLAVE_NAME, 'gen_rpc_slave').
-define(SLAVE_NAME1, 'gen_rpc_slave1').
-define(SLAVE_NAME2, 'gen_rpc_slave2').

%%% Application setup
-define(set_application_environment(),
    [application:set_env(Application, Key, Value, [{persistent, true}]) || {Application, Key, Value} <-
        [{sync, growl, none},
        {sync, log, none},
        {sasl, errlog_type, error},
        {sasl, error_logger_mf_dir, false},
        {gen_rpc, connect_timeout, 500},
        {gen_rpc, send_timeout, 500},
        {lager, colored, true},
        {lager, handlers, [
            {lager_console_backend, [info, {lager_default_formatter, ["[", date, " ", time, "] severity=", severity, " module=", {module, "gen_rpc"}, " pid=\"", pid, "\" ", message, "\n"]}]},
            {lager_common_test_backend, [error, {lager_default_formatter, ["[", date, " ", time, "] severity=", severity, " module=", {module, "gen_rpc"}, " pid=\"", pid, "\" ", message, "\n"]}]}
        ]}
    ]]
).

-define(restart_application(),
    begin
        ok = application:stop(?APP),
        ok = application:unload(?APP),
        ok = application:start(?APP)
    end
).
