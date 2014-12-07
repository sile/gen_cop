-module(gen_cop).

-export([start_link/4]).

-type options() :: []. % max queue length

-spec start_link(gen_cop_connection:connection(), gen_cop_codec:codec(), [gen_cop_handler:spec()], options()) ->
                        {ok, pid()} | {error, Reason} when
      Reason :: term().
start_link(Connection, Codec, HandlerSpecs, Options) ->
    gen_cop_session:start_link(Connection, Codec, HandlerSpecs, Options).
