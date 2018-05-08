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
-export_type([logi_logger/0]).

-export([reply/2]).
-export([send/2]).
-export([cast/2]).
-export([call/2, call/3]).

-export([get_logger/1]).

-export([which_handlers/1]).

%% -export([add_handler/4, remove_handler/2]).

-export([default_on_owner_down/2]).

-export_type([on_owner_down/0]).

-export_type([from/0, data/0, info/0, context/0, message/0, request/0]).
%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type from() :: term().
-type data() :: term().
-type info() :: term().
-type context() :: gen_cop_context:context().
-type message() :: term().
-type request() :: term().

-type codec()         :: gen_cop_codec:codec().
-type handler_specs() :: [handler_spec()].
-type handler_spec()  :: gen_cop_handler:spec().

-type start_opts() :: [start_opt()].
-type start_opt() :: {link, boolean()}
                   | {name, otp_name()}
                   | {ack_fun, ack_fun()}
                   | {async, boolean()}
                   | {owner, pid()}
                   | {on_owner_down, on_owner_down()}
                   | {spawn_opt, [term()]} % TODO: more specific typespec
                   | {timeout, timeout()}
                   | {debug, [term()]} % TODO: more specific typespec
                   | {logger, logi_logger()}.
%% TODO: doc

-type logi_logger() :: term().
%% logi:context() in v0.1.12, logi:logger() in v0.5.x

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

-type on_owner_down() :: fun ((OwerPid::pid(), OwnerExitReason::term()) -> ExitReason::term()).

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
    Handlers = lists:map(fun gen_cop_handler:make_instance_if_need/1, HandlerSpecs),
    gen_cop_server:start({Socket, Codec, Handlers, Options}).

default_on_owner_down(Pid, Reason) ->
    {owner_down, Pid, Reason}.

send(Data, Context) ->
    gen_cop_server:send(Data, Context).

-spec cast(otp_ref(), term()) -> ok.
cast(ServerRef, Request) ->
    gen_cop_server:cast(ServerRef, Request).

-spec call(otp_ref(), term()) -> term().
call(ServerRef, Request) ->
    call(ServerRef, Request, 5000).

-spec call(otp_ref(), term(), timeout()) -> term().
call(ServerRef, Request, Timeout) ->
    gen_cop_server:call(ServerRef, Request, Timeout).

reply(From, Reply) ->
    _ = gen_server:reply(From, Reply),
    ok.

%% TODO: rename: get_default_logger
-spec get_logger(otp_ref()) -> logi_logger().
get_logger(ServerRef) ->
    call(ServerRef, get_logger).

-spec which_handlers(otp_ref()) -> [{gen_cop_handler:id(), module()}].
which_handlers(ServerRef) ->
    gen_cop_server:which_handlers(ServerRef).

% TODO: return: {ok, _} | {error, _, _}
%% add_handler(Pos, Mod, State, Context) ->
%%     gen_cop_server:add_handler(Pos, Mod, State, Context).

%% remove_handler(Reason, Context) ->
%%     gen_cop_server:remove_handler(Reason, Context).
