-module(gen_cop_debug).

-export([start/0, rtmp_client/2]).

start() ->
    {ok, Socket} = gen_tcp:connect("localhost", 22, [{active, false}, binary]),
    Conn = {gen_tcp, Socket},
    Codec = {dummy_codec, hoge},
    HandlerSpecs = [],
    gen_cop:start_link(Conn, Codec, HandlerSpecs, []).

rtmp_client(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, false}, binary]),
    Codec = rtmp_codec:new(),
    HandlerSpecs =
        [
         {rtmp_cop_handler_client_control, []},
         {rtmp_cop_handler_control, [client]}
        ],
    gen_cop:start_link(Socket, Codec, HandlerSpecs, [{name, {local, hoge}}]).
