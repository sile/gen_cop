%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Protocol Session Context
%% @private
%%
%% errorを持ち回すよりはthrowを使った方が効率的かもしれない
-module(gen_cop_context).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/3]).
-export([get_socket/1]).
-export([send/2]).
-export([recv/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([flush_send_queue/1]).

-export([delegate_data/3]).
-export([delegate_call/4]).
-export([delegate_cast/3]).
-export([delegate_info/3]).
%% TODO: add_handler, remove_handler, swap_handler

-export_type([context/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(CONTEXT, ?MODULE).

-record(?CONTEXT,
        {
          socket :: inet:socket(),
          codec :: gen_cop_codec:codec(),
          handlers = [] :: [gen_cop_handler:handler()],
          send_queue = [] :: [term()] % TODO: type
        }).

-opaque context() :: #?CONTEXT{}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec init(inet:socket(), gen_cop_codec:codec(), [gen_cop_handler:spec()]) -> {ok, context()} | {error, Reason::term()}.
init(Socket, Codec, HandlerSpecs) ->
    Context0 = #?CONTEXT{socket = Socket, codec = Codec},
    case handlers_init(lists:reverse(HandlerSpecs), Context0) of
        {error, Reason, Context1} ->
            _ = handlers_terminate(Reason, Context1),
            {error, Reason};
        {ok, Context1} ->
            {ok, Context1}
    end.

-spec send(gen_cop:data(), context()) -> context().
send(Data, Context) ->
    Context#?CONTEXT{send_queue = [Data | Context#?CONTEXT.send_queue]}.

-spec recv(binary(), context()) -> {ok, context()} | {error, Reason::term(), context()}. % XXX: name
recv(Bin, Context) ->
    case gen_cop_codec:decode(Bin, Context#?CONTEXT.codec) of
        {error, Reason, Codec} -> {error, Reason, Context#?CONTEXT{codec = Codec}};
        {ok, Messages, Codec}  -> handle_messages(Messages, Context#?CONTEXT{codec = Codec})
    end.

-spec delegate_data(gen_cop:data(), gen_cop_handler:state(), context()) ->
                           gen_cop_handler:handle_result(gen_cop_handler:handler()).
delegate_data(Data, State, Context0) ->
    case handlers_handle_message(Data, Context0) of
        {error, Reason, Context1} -> {stop, Reason, State, Context1};
        {ok, Context1}            -> {ok, State, Context1}
    end.

-spec delegate_call(term(), gen_cop:from(), gen_cop_handler:state(), context()) ->
                           gen_cop_handler:handle_result(gen_cop_handler:handler()).
delegate_call(Request, From, State, Context0) ->
    case handle_call(Request, From, Context0) of
        {error, Reason, Context1} -> {stop, Reason, State, Context1};
        {ok, Context1}            -> {ok, State, Context1}
    end.

-spec delegate_cast(term(), gen_cop_handler:state(), context()) ->
                           gen_cop_handler:handle_result(gen_cop_handler:handler()).
delegate_cast(Request, State, Context0) ->
    case handle_cast(Request, Context0) of
        {error, Reason, Context1} -> {stop, Reason, State, Context1};
        {ok, Context1}            -> {ok, State, Context1}
    end.

-spec delegate_info(term(), gen_cop_handler:state(), context()) ->
                           gen_cop_handler:handle_result(gen_cop_handler:handler()).
delegate_info(Info, State, Context0) ->
    case handle_info(Info, Context0) of
        {error, Reason, Context1} -> {stop, Reason, State, Context1};
        {ok, Context1}            -> {ok, State, Context1}
    end.

-spec handle_call(term(), gen_cop:from(), context()) -> {ok, context()} | {error, Reason::term(), context()}.
handle_call(Request, From, Context = #?CONTEXT{handlers = []}) ->
    {error, {unhandled_call, Request, From}, Context};
handle_call(Request, From, Context0 = #?CONTEXT{handlers = [H | Handlers]}) ->
    adjust_handle_result(gen_cop_hander:handle_call(Request, From, H, Context0#?CONTEXT{handlers = Handlers})).

-spec handle_cast(term(), context()) -> {ok, context()} | {error, Reason::term(), context()}.
handle_cast(Request, Context = #?CONTEXT{handlers = []}) ->
    {error, {unhandled_cast, Request}, Context};
handle_cast(Request, Context0 = #?CONTEXT{handlers = [H | Handlers]}) ->
    adjust_handle_result(gen_cop_handler:handle_cast(Request, H, Context0#?CONTEXT{handlers = Handlers})).

-spec handle_info(term(), context()) -> {ok, context()} | {error, Reason::term(), context()}.
handle_info(Info, Context = #?CONTEXT{handlers = []}) ->
    {error, {unhandled_info, Info}, Context};
handle_info(Info, Context0 = #?CONTEXT{handlers = [H | Handlers]}) ->
    adjust_handle_result(gen_cop_handler:handle_info(Info, H, Context0#?CONTEXT{handlers = Handlers})).

-spec flush_send_queue(context()) -> {ok, iodata(), context()} | {error, Reason::term(), context()}. % XXX: name
flush_send_queue(Context0 = #?CONTEXT{send_queue = Queue}) ->
    Result = gen_cop_codec:encode(lists:reverse(Queue), Context0#?CONTEXT.codec),
    Context1 = Context0#?CONTEXT{codec = element(3, Result), send_queue = []},
    setelement(3, Result, Context1).

-spec get_socket(context()) -> inet:socket().
get_socket(Context) ->
    gen_cop_session:get_socket(Context).

-spec terminate(term(), context()) -> context().
terminate(Reason, Context) ->
    handlers_terminate(Reason, Context).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec handlers_init([gen_cop_handler:spec()], context()) -> {ok, context()} | {error, Reason::term(), context()}.
handlers_init([], Context) ->
    {ok, Context};
handlers_init([Spec | HandlerSpecs], Context0) ->
    case gen_cop_handler:init(Spec, Context0) of
        {stop, Reason, Context1} -> {error, Reason, Context1};
        {ok, Handler, Context1}  ->
            Context2 = Context1#?CONTEXT{handlers = [Handler | Context1#?CONTEXT.handlers]},
            handlers_init(HandlerSpecs, Context2)
    end.

-spec handlers_terminate(term(), context()) -> context().
handlers_terminate(_Reason, Context = #?CONTEXT{handlers = []}) ->
    Context;
handlers_terminate(Reason, Context0 = #?CONTEXT{handlers = [Handler | Handlers]}) ->
    Context1 = Context0#?CONTEXT{handlers = Handlers},
    Context2 = gen_cop_handler:terminate(Reason, Handler, Context1),
    handlers_terminate(Reason, Context2).

-spec handle_messages([gen_cop:data()], context()) -> {ok, context()} | {error, Reason::term(), context()}.
handle_messages([], Context) ->
    {ok, Context};
handle_messages([Msg | Messages], Context0) ->
    case handlers_handle_message(Msg, Context0) of
        {error, Reason, Context1} -> {error, Reason, Context1};
        {ok, Context1}            -> handle_messages(Messages, Context1)
    end.

-spec handlers_handle_message(gen_cop:data(), context()) -> {ok, context()} | {error, Reason::term(), context()}.
handlers_handle_message(Msg, Context = #?CONTEXT{handlers = []}) ->
    {error, {unhandled_message, Msg}, Context};
handlers_handle_message(Msg, Context0 = #?CONTEXT{handlers = [Handler0 | Handlers]}) ->
    adjust_handle_result(gen_cop_handler:handle_data(Msg, Handler0, Context0#?CONTEXT{handlers = Handlers})).

-spec adjust_handle_result(HandleResult) -> {ok, context()} | {error, Reason, context()} when
      HandleResult :: {ok, gen_cop_handler:handler(), context()}
                    | {stop, Reason, gen_cop_handler:handler(), context()},
      Reason :: term().
adjust_handle_result({stop, Reason, Handler, Context}) ->
    {error, Reason, Context#?CONTEXT{handlers = [Handler | Context#?CONTEXT.handlers]}};
adjust_handle_result({ok, Handler, Context}) ->
    {ok, Context#?CONTEXT{handlers = [Handler | Context#?CONTEXT.handlers]}}.
