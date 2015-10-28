# gen_rpc

[![Build Status](https://travis-ci.org/priestjim/gen_rpc.svg)](https://travis-ci.org/priestjim/gen_rpc) [![Coverage Status](https://coveralls.io/repos/priestjim/gen_rpc/badge.png?branch=master)](https://coveralls.io/r/priestjim/gen_rpc?branch=master)

An Erlang RPC library for out-of-band messaging

It is a library designed to solve when the amount of messages exchanged between erlang nodes overwhelm all out-of-the-box solutions (such as rpc/rex and remote spawn) Erlang offers and the stability of your infrastructure is threatened.

