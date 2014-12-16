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
%% TODO: add_handler, remove_handler, swap_handler

-export_type([context/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(CONTEXT, ?MODULE).

%% TODO: delete
-define(LOCATION, [{module, ?MODULE}, {line, ?LINE}, {pid, self()}]).
-define(MAKE_EX_REASON(ExClass, ExReason, MFArgs),
        {'EXIT', [{exception, ExClass, ExReason},
                  {trace, erlang:get_stacktrace()},
                  {location, ?LOCATION},
                  {mfargs, MFArgs}]}).

-record(?CONTEXT,
        {
          socket :: inet:socket(),
          codec :: gen_cop_codec:codec(),
          done_handlers = [] :: [gen_cop_handler:handler()],
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

-spec handle_call(term(), gen_cop:from(), context()) -> {ok, context()} | {error, Reason::term(), context()}.
handle_call(Request, From, Context = #?CONTEXT{handlers = []}) ->
    {error, {unhandled_call, Request, From}, fix_handlers(Context)};
handle_call(Request, From, Context0 = #?CONTEXT{handlers = [H | Handlers]}) ->
    case handler_handle_call(Request, From, H, Context0#?CONTEXT{handlers = Handlers}) of
        {stop, Reason, Context1} -> {error, Reason, fix_handlers(Context1)};
        {delegate, Context1}     -> handle_call(Request, From, Context1);
        {ok, Context1}           -> {ok, fix_handlers(Context1)}
    end.

-spec handle_cast(term(), context()) -> {ok, context()} | {error, Reason::term(), context()}.
handle_cast(Request, Context = #?CONTEXT{handlers = []}) ->
    {error, {unhandled_cast, Request}, fix_handlers(Context)};
handle_cast(Request, Context0 = #?CONTEXT{handlers = [H | Handlers]}) ->
    case handler_handle_cast(Request, H, Context0#?CONTEXT{handlers = Handlers}) of
        {stop, Reason, Context1} -> {error, Reason, fix_handlers(Context1)};
        {delegate, Context1}     -> handle_cast(Request, Context1);
        {ok, Context1}           -> {ok, fix_handlers(Context1)}
    end.

-spec handle_info(term(), context()) -> {ok, context()} | {error, Reason::term(), context()}.
handle_info(Info, Context = #?CONTEXT{handlers = []}) ->
    {error, {unhandled_info, Info}, fix_handlers(Context)};
handle_info(Info, Context0 = #?CONTEXT{handlers = [H | Handlers]}) ->
    case handler_handle_info(Info, H, Context0#?CONTEXT{handlers = Handlers}) of
        {stop, Reason, Context1} -> {error, Reason, fix_handlers(Context1)};
        {delegate, Context1}     -> handle_info(Info, Context1);
        {ok, Context1}           -> {ok, fix_handlers(Context1)}
    end.

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
    case handler_init(Spec, Context0) of
        {error, Reason, Context1} -> {error, Reason, Context1};
        {ok, Context1}            -> handlers_init(HandlerSpecs, Context1)
    end.

-spec handler_init(gen_cop_handler:spec(), context()) -> {ok, context()} | {error, Reason::term(), context()}.
handler_init({Mod, Arg}, Context0) ->
    try
        case Mod:init(Arg, Context0) of
            {stop, Reason, Context1} -> {error, Reason, Context1};
            {ok, State, Context1}    ->
                Context2 = Context1#?CONTEXT{handlers = [{Mod, State} | Context1#?CONTEXT.handlers]},
                {ok, Context2}
        end
    catch
        ExClass:ExReason ->
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {Mod, init, [Arg, Context0]}), Context0}
    end.

-spec handlers_terminate(term(), context()) -> context().
handlers_terminate(_Reason, Context = #?CONTEXT{handlers = []}) ->
    Context;
handlers_terminate(Reason, Context0 = #?CONTEXT{handlers = [Handler | Handlers]}) ->
    Context1 = Context0#?CONTEXT{handlers = Handlers},
    Context2 = handler_terminate(Reason, Handler, Context1),
    handlers_terminate(Reason, Context2).

-spec handler_terminate(term(), gen_cop_handler:handler(), context()) -> context().
handler_terminate(Reason, {Mod, State}, Context0) ->
    try
        Mod:terminate(Reason, State, Context0)
    catch
        ExClass:ExReason ->
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {Mod, terminate, [Reason, State, Context0]}), Context0}
    end.

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
    {error, {unhandled_message, Msg}, fix_handlers(Context)};
handlers_handle_message(Msg, Context0 = #?CONTEXT{handlers = [H | Handlers]}) ->
    case handler_handle_message(Msg, H, Context0#?CONTEXT{handlers = Handlers}) of
        {stop, Reason, Context1} -> {error, Reason, fix_handlers(Context1)};
        {delegate, Context1}     -> handlers_handle_message(Msg, Context1);
        {ok, Context1}           -> {ok, fix_handlers(Context1)}
    end.

-spec handler_handle_message(gen_cop:data(), gen_cop_handler:handler(), context()) ->
                                 {ok, context()} | {error, Reason::term(), context()}.
handler_handle_message(Msg, {Mode, State0}, Context0) ->
    try
        case Mode:handle_data(Msg, State0, Context0) of
            {stop, Reason, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mode, State1} | Context1#?CONTEXT.done_handlers]},
                {error, Reason, Context2};
            {delegate, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mode, State1} | Context1#?CONTEXT.done_handlers]},
                {delegate, Context2};
            {noreply, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mode, State1} | Context1#?CONTEXT.done_handlers]},
                {ok, Context2}
        end
    catch
        ExClass:ExReason ->
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {Mode, handle_data, [Msg, State0, Context0]}), Context0}
    end.


%% XXX: name
-spec fix_handlers(context()) -> context().
fix_handlers(Context) ->
    Context#?CONTEXT{handlers = lists:reverse(Context#?CONTEXT.done_handlers, Context#?CONTEXT.handlers),
                     done_handlers = []}.

-spec handler_handle_call(term(), gen_cop:from(), gen_cop_handler:handler(), context()) ->
                                 {ok, context()} | {error, Reason::term(), context()}.
handler_handle_call(Request, From, {Mod, State0}, Context0) ->
    try
        case Mod:handle_call(Request, From, State0, Context0) of
            {stop, Reason, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mod, State1} | Context1#?CONTEXT.done_handlers]},
                {error, Reason, Context2};
            {stop, Reply, Reason, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mod, State1} | Context1#?CONTEXT.done_handlers]},
                ok = gen_cop:reply(From, Reply),
                {error, Reason, Context2};
            {delegate, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mod, State1} | Context1#?CONTEXT.done_handlers]},
                {delegate, Context2};
            {noreply, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mod, State1} | Context1#?CONTEXT.done_handlers]},
                {ok, Context2};
            {reply, Reply, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mod, State1} | Context1#?CONTEXT.done_handlers]},
                ok = gen_cop:reply(From, Reply),
                {ok, Context2}
        end
    catch
        ExClass:ExReason ->
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {Mod, handle_call, [Request, From, State0, Context0]}), Context0}
    end.

-spec handler_handle_cast(term(), gen_cop_handler:handler(), context()) ->
                                 {ok, context()} | {error, Reason::term(), context()}.
handler_handle_cast(Request, {Mod, State0}, Context0) ->
    try
        case Mod:handle_cast(Request, State0, Context0) of
            {stop, Reason, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mod, State1} | Context1#?CONTEXT.done_handlers]},
                {error, Reason, Context2};
            {delegate, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mod, State1} | Context1#?CONTEXT.done_handlers]},
                {delegate, Context2};
            {noreply, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mod, State1} | Context1#?CONTEXT.done_handlers]},
                {ok, Context2}
        end
    catch
        ExClass:ExReason ->
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {Mod, handle_cast, [Request, State0, Context0]}), Context0}
    end.

-spec handler_handle_info(term(), gen_cop_handler:handler(), context()) ->
                                 {ok, context()} | {error, Reason::term(), context()}.
handler_handle_info(Info, {Mod, State0}, Context0) ->
    try
        case Mod:handle_info(Info, State0, Context0) of
            {stop, Reason, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mod, State1} | Context1#?CONTEXT.done_handlers]},
                {error, Reason, Context2};
            {delegate, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mod, State1} | Context1#?CONTEXT.done_handlers]},
                {delegate, Context2};
            {noreply, State1, Context1} ->
                Context2 = Context1#?CONTEXT{done_handlers = [{Mod, State1} | Context1#?CONTEXT.done_handlers]},
                {ok, Context2}
        end
    catch
        ExClass:ExReason ->
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {Mod, handle_info, [Info, State0, Context0]}), Context0}
    end.
