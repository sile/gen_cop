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

-export([ok/1, ok/2, reply/3, reply/4]).
-export([stop/2, stop/3]).
-export([raise/3, raise/4]).

-export([delegate_data/3]).
-export([delegate_call/4]).
-export([delegate_cast/3]).
-export([delegate_info/3]).
%% TODO: add_handler, remove_handler, swap_handler

-export_type([context/0]).
-export_type([handler_result/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(CONTEXT, ?MODULE).

-record(?CONTEXT,
        {
          socket :: inet:socket(),
          codec :: gen_cop_codec:codec(),
          handlers = [] :: [gen_cop_handler:handler()], % XXX: non-empty check
          done_handlers = [] :: [gen_cop_handler:handler()],
          send_queue = [] :: [term()] % TODO: type
        }).

-opaque context() :: #?CONTEXT{}.

-type handler_result() :: {ok, context()} | {stop, Reason::term(), context()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec init(inet:socket(), gen_cop_codec:codec(), [gen_cop_handler:uninitialized_handler()]) ->
                  {ok, context()} | {stop, Reason::term()}.
init(Socket, Codec, Handlers) ->
    Context0 = #?CONTEXT{socket = Socket, codec = Codec},
    case handlers_init(lists:reverse(Handlers), Context0) of
        {stop, Reason, Context1} ->
            _ = handlers_terminate(Reason, Context1),
            {stop, Reason};
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

-spec delegate_data(gen_cop:data(), gen_cop_handler:state(), context()) -> handler_result().
delegate_data(Data, State, Context) ->
    handle_data(Data, next_handler(State, Context)).

-spec delegate_call(term(), gen_cop:from(), gen_cop_handler:state(), context()) -> handler_result().
delegate_call(Request, From, State, Context) ->
    handle_call(Request, From, next_handler(State, Context)).

-spec delegate_cast(term(), gen_cop_handler:state(), context()) -> handler_result().
delegate_cast(Request, State, Context) ->
    handle_cast(Request, next_handler(State, Context)).

-spec delegate_info(term(), gen_cop_handler:state(), context()) -> handler_result().
delegate_info(Info, State, Context) ->
    handle_info(Info, next_handler(State, Context)).

-spec handle_call(term(), gen_cop:from(), context()) -> handler_result().
handle_call(Request, From, Context = #?CONTEXT{handlers = []}) ->
    stop({unhandled_call, Request, From}, Context);
handle_call(Request, From, Context = #?CONTEXT{handlers = [Handler | _]}) ->
    gen_cop_hander:handle_call(Request, From, Handler, Context).

-spec handle_cast(term(), context()) -> handler_result().
handle_cast(Request, Context = #?CONTEXT{handlers = []}) ->
    stop({unhandled_cast, Request}, Context);
handle_cast(Request, Context = #?CONTEXT{handlers = [Handler | _]}) ->
    gen_cop_handler:handle_cast(Request, Handler, Context).

-spec handle_info(term(), context()) -> handler_result().
handle_info(Info, Context = #?CONTEXT{handlers = []}) ->
    stop({unhandled_info, Info}, Context);
handle_info(Info, Context = #?CONTEXT{handlers = [Handler | _]}) ->
    gen_cop_handler:handle_info(Info, Handler, Context).

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

%% XXX: tmp
-spec raise(Class, Reason, context()) -> {stop, Reason, context()} when
      Class :: throw | error | exit,
      Reason :: term().
raise(Class, Reason, Context) ->
    raise(Class, Reason, erlang:get_stacktrace(), Context).

-spec raise(Class, Reason, StackTrace, context()) -> {stop, Reason, context()} when
      Class :: throw | error | exit,
      Reason :: term(),
      StackTrace :: [term()]. % TODO:
raise(Class, Reason, StackTrace, Context) ->
    stop({'EXIT', {Class, Reason, StackTrace}}, Context).

-spec stop(Reason, context()) -> {stop, Reason, context()} when
      Reason :: term().
stop(Reason, Context) ->
    {stop, Reason, fix_handlers(Context)}.

-spec stop(Reason, gen_cop_handler:state(), context()) -> {stop, Reason, context()} when
      Reason :: term().
stop(Reason, State, Context) ->
    stop(Reason, update_state(State, Context)).

-spec ok(gen_cop_handler:state(), context()) -> {ok, context()}.
ok(State, Context) ->
    ok(update_state(State, Context)).

-spec ok(context()) -> {ok, context()}.
ok(Context) ->
    {ok, fix_handlers(Context)}.

-spec reply(gen_cop:from(), term(), context()) -> {ok, context()}.
reply(From, Result, Context) ->
    _ = gen_cop:reply(From, Result),
    ok(Context).

-spec reply(gen_cop:from(), term(), gen_cop_handler:state(), context()) -> {ok, context()}.
reply(From, Result, State, Context) ->
    reply(From, Result, update_state(State, Context)).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec handlers_init([gen_cop_hander:uninitialized_handler()], context()) -> handler_result().
handlers_init([], Context) ->
    {ok, Context};
handlers_init([Handler | Rest], Context0) ->
    case gen_cop_handler:init(Handler, Context0#?CONTEXT{handlers = [Handler | Context0#?CONTEXT.handlers]}) of
        {stop, Reason, Context1} -> {stop, Reason, Context1};
        {ok, Context1}           -> handlers_init(Rest, Context1)
    end.

-spec handlers_terminate(term(), context()) -> context().
handlers_terminate(_Reason, Context = #?CONTEXT{handlers = []}) ->
    Context;
handlers_terminate(Reason, Context0 = #?CONTEXT{handlers = [Handler | RestHandlers]}) ->
    ok = case gen_cop_handler:terminate(Reason, Handler, Context0) of
             {stop, _, Context1} -> ok; % XXX: note
             {ok, Context1}      -> ok
         end,
    Context2 = Context1#?CONTEXT{handlers = RestHandlers},
    handlers_terminate(Reason, Context2).

-spec handle_messages([gen_cop:data()], context()) -> handler_result().
handle_messages([], Context) ->
    {ok, Context};
handle_messages([Msg | Messages], Context0) ->
    case handle_data(Msg, Context0) of
        {stop, Reason, Context1} -> {stop, Reason, Context1};
        {ok, Context1}            -> handle_messages(Messages, Context1)
    end.

-spec handle_data(gen_cop:data(), context()) -> handler_result().
handle_data(Data, Context = #?CONTEXT{handlers = []}) ->
    stop({unhandled_data, Data}, Context);
handle_data(Data, Context = #?CONTEXT{handlers = [Handler | _]}) ->
    gen_cop_handler:handle_data(Data, Handler, Context).

-spec update_state(gen_cop_handler:state(), context()) -> context().
update_state(State, Context = #?CONTEXT{handlers = [{Header, _} | Handlers]}) ->
    Context#?CONTEXT{handlers = [{Header, State} | Handlers]}.

-spec next_handler(gen_cop_handler:state(), context()) -> context().
next_handler(State, Context = #?CONTEXT{handlers = [{Header, _} | Handlers], done_handlers = Dones}) ->
    Context#?CONTEXT{handlers = Handlers, done_handlers = [{Header, State} | Dones]}.

-spec fix_handlers(context()) -> context().
fix_handlers(Context = #?CONTEXT{done_handlers = Dones, handlers = Tail}) ->
    Context#?CONTEXT{handlers = lists:reverse(Dones, Tail), done_handlers = []}.
