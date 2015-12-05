%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-define(APP, gen_rpc).

%%% Default TCP options
-define(DEFAULT_TCP_OPTS, [binary, {packet,4},
        {nodelay,true}, % Send our requests immediately
        {send_timeout_close,true}, % When the socket times out, close the connection
        {delay_send,true}, % Scheduler should favor big batch requests
        {linger,{true,2}}, % Allow the socket to flush outgoing data for 2" before closing it - useful for casts
        {reuseaddr,true}, % Reuse local port numbers
        {keepalive,true}, % Keep our channel open
        {tos,72}, % Deliver immediately
        {active,false}]). % Retrieve data from socket upon request

%%% Default SSL options
-define(DEFAULT_SSL_OPTS, [{cacertfile,"/path/to/testca/cacert.pem"},
        {certfile,"/path/to/server/cert.pem"},
        {keyfile,"/path/to/server/key.pem"},
        {verify,verify_peer},
        {fail_if_no_peer_cert,false},
        {versions, ['tlsv1.2', 'tlsv1.1', 'tlsv1']},
        {dhfile, "/path/to/testca/dh-params.pem"},
        {ciphers, ["ECDHE-ECDSA-AES256-GCM-SHA384","ECDHE-RSA-AES256-GCM-SHA384",
                   "ECDHE-ECDSA-AES256-SHA384","ECDHE-RSA-AES256-SHA384", "ECDHE-ECDSA-DES-CBC3-SHA",
                   "ECDH-ECDSA-AES256-GCM-SHA384","ECDH-RSA-AES256-GCM-SHA384","ECDH-ECDSA-AES256-SHA384",
                   "ECDH-RSA-AES256-SHA384","DHE-DSS-AES256-GCM-SHA384","DHE-DSS-AES256-SHA256",
                   "AES256-GCM-SHA384","AES256-SHA256","ECDHE-ECDSA-AES128-GCM-SHA256",
                   "ECDHE-RSA-AES128-GCM-SHA256","ECDHE-ECDSA-AES128-SHA256","ECDHE-RSA-AES128-SHA256",
                   "ECDH-ECDSA-AES128-GCM-SHA256","ECDH-RSA-AES128-GCM-SHA256","ECDH-ECDSA-AES128-SHA256",
                   "ECDH-RSA-AES128-SHA256","DHE-DSS-AES128-GCM-SHA256","DHE-DSS-AES128-SHA256",
                   "AES128-GCM-SHA256","AES128-SHA256","ECDHE-ECDSA-AES256-SHA",
                   "ECDHE-RSA-AES256-SHA","DHE-DSS-AES256-SHA","ECDH-ECDSA-AES256-SHA",
                   "ECDH-RSA-AES256-SHA","AES256-SHA","ECDHE-ECDSA-AES128-SHA",
                   "ECDHE-RSA-AES128-SHA","DHE-DSS-AES128-SHA","ECDH-ECDSA-AES128-SHA",
                   "ECDH-RSA-AES128-SHA","AES128-SHA"]},
        {secure_renegotiate, true},
        {reuse_sessions, true},
        {honor_cipher_order, true},
        {max_connections, infinity}]).






