-module(gen_cop_debug).

-export([start/0]).

start() ->
    {ok, Socket} = gen_tcp:connect("localhost", 22, [{active, false}, binary]),
    Conn = {gen_tcp, Socket},
    Codec = {dummy_codec, hoge},
    HandlerSpecs = [],
    gen_cop:start_link(Conn, Codec, HandlerSpecs, []).
