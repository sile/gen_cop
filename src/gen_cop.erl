%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Interface to `gen_cop' modules and processes
-module(gen_cop).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start/3, start/4, start_link/3, start_link/4]).

-export_type([codec/0]).
-export_type([handler_spec/0, handler_specs/0]).
-export_type([start_opt/0, start_opts/0]).
-export_type([start_err/0]).
-export_type([ack_fun/0]).
-export_type([otp_name/0, otp_ref/0]).

-export([reply/2]).
-export([send/2]).

-export([add_handler/4, remove_handler/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type codec()         :: gen_cop_codec:codec().
-type handler_specs() :: [handler_spec()].
-type handler_spec()  :: gen_cop_handler:spec().

-type start_opts() :: [start_opt()].
-type start_opt() :: {link, boolean()}
                   | {name, otp_name()}
                   | {ack_fun, ack_fun()}
                   | {async, boolean()}
                   | {spawn_opt, [term()]} % TODO: more specific typespec
                   | {timeout, timeout()}
                   | {debug, [term()]}. % TODO: more specific typespec
%% TODO: doc

-type start_err() :: {already_started, pid()} | timeout | term().

-type ack_fun() :: fun (() -> any()). % TODO: doc

-type otp_name() :: {local, Name :: atom()}
                  | {global, Name :: term()}
                  | {via, module(), Name :: term()}.

-type otp_ref() :: (Name :: atom())
                 | {Name :: atom(), node()}
                 | {global, Name :: term()}
                 | {via, module(), Name :: term()}
                 | pid().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @equiv start_link(Socket, Codec, HandlerSpecs, [])
-spec start_link(inet:socket(), codec(), handler_specs()) -> {ok, pid()} | {error, start_err()}.
start_link(Socket, Codec, HandlerSpecs) ->
    start_link(Socket, Codec, HandlerSpecs, []).

%% @equiv start(Socket, Codec, HandlerSpecs, [{link, true} | Options])
-spec start_link(inet:socket(), codec(), handler_specs(), start_opts()) -> {ok, pid()} | {error, start_err()}.
start_link(Socket, Codec, HandlerSpecs, Options) ->
    start(Socket, Codec, HandlerSpecs, [{link, true} | Options]).

%% @equiv start(Socket, Codec, HandlerSpecs, [])
-spec start(inet:socket(), codec(), handler_specs()) -> {ok, pid()} | {error, start_err()}.
start(Socket, Codec, HandlerSpecs) ->
    start(Socket, Codec, HandlerSpecs, []).

-spec start(inet:socket(), codec(), handler_specs(), start_opts()) -> {ok, pid()} | {error, start_err()}.
start(Socket, Codec, HandlerSpecs, Options) ->
    Handlers = lists:map(fun gen_cop_handler:make_instance/1, HandlerSpecs),
    gen_cop_server:start({Socket, Codec, Handlers, Options}).

reply(_, _) ->
    ok.

send(Data, Context) ->
    gen_cop_server:send(Data, Context).

% TODO: return: {ok, _} | {error, _, _}
add_handler(Pos, Mod, State, Context) ->
    gen_cop_server:add_handler(Pos, Mod, State, Context).

remove_handler(Reason, Context) ->
    gen_cop_server:remove_handler(Reason, Context).
