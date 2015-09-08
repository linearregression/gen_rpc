gen_rpc
===
An Erlang/Elixir RPC library for out-of-band messaging

It is a library designed to solve when the amount of messages exchanged between erlang nodes overwhelm all out-of-the-box solutions (such as rpc/rex and remote spawn) Erlang offers and the stability of your infrastructure is threatened.

Testing
---
Test code under test directory. It uses Erlang builtin [Common Test Framework](http://www.erlang.org/doc/apps/common_test/basics_chapter.html). 
This framework comes with full distribution although some off-the-shelf distribution may exclude this. 
* To invoke testing

``` make test ```

* To invoke test with code coverge

``` make testcov ```

Check makefile for details. 

Code Coverge
---
Most important file is test/gen_rpc.coverspec & rebar.config

gen_rpc.coverspec details which modules to instrument, details levels etc. It uses Erlang built in code coverage tool.

rebar.config (look at ct_opts and tuples with cover e.g. {cover_enabled, true}. ). 
Some settings are for rebar to recognize common test, but many are pass through to underlying Common Test Framework.
* Note: Rebar3 is aimed to be backward compatible with older versions of rebar. You may need to refer to both for complete picture.

Please refer:
* [Code Coverge](http://www.erlang.org/doc/apps/common_test/cover_chapter.html)
* [Rebar3](https://www.rebar3.org/docs/configuration)
* [Rebar](https://github.com/rebar/rebar/wiki) and the sample
* [Common Test Framework Configurations] (http://www.erlang.org/doc/man/common_test.html)

