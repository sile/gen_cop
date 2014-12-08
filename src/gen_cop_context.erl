-module(gen_cop_context).

-export([get_socket/1]).

get_socket(Context) ->
    gen_cop_session:get_socket(Context).
