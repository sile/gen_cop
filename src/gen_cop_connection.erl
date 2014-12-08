-module(gen_cop_connection).

-export([controlling_process/2, setopts/2, send/2]).

controlling_process({Mod, State}, Pid) ->
    Mod:controlling_process(State, Pid).

setopts({_Mod, State}, Opts) ->
    inet:setopts(State, Opts). % TODO:

send({Mod, State}, Data) ->
    Mod:send(State, Data).
