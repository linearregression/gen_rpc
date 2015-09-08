gen_rpc
===
An Erlang/Elixir RPC library for out-of-band messaging

It is a library designed to solve when the amount of messages exchanged between erlang nodes overwhelm all out-of-the-box solutions (such as rpc/rex and remote spawn) Erlang offers and the stability of your infrastructure is threatened.

Testing
---
Test code under test directory. It uses Erlang builtin [Common Test Framework](http://www.erlang.org/doc/apps/common_test/basics_chapter.html). 
It comes with full distribution though some off-the-shelf distribution may exclude this. 
* To invoke testing

``` make test ```

* To invoke test with code coverge

``` make testcov ```

Check makefile for details. 

Code Coverge
---
Most important file is test/gen_rpc.coverspec
It details which modules to instrument, details levels etc. It uses Erlang built in code coverage tool.
Please refer to [Code Coverge](http://www.erlang.org/doc/apps/common_test/cover_chapter.html)
