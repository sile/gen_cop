%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Protocol Session Process
%% @private
-module(gen_cop_session).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Application Internal API
%%----------------------------------------------------------------------------------------------------------------------
-export([send/2]). % TODO: move to gen_cop_context module
-export([get_socket/1]).
-export([add_handler/4, remove_handler/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/5]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'sys' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([system_continue/3, system_terminate/4, system_code_change/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-record(state,
        {
          socket :: inet:socket(),
          codec :: gen_cop_codec:codec(),
          done_handlers = [] :: [gen_cop_handler:handler()],
          handlers = [] :: [gen_cop_handler:handler()],
          send_queue = [] :: [term()], % TODO: type
          request_queue = [] :: [term()] % TODO: type
        }).

-type sync_fun() :: fun ((Result::term()) -> any()). % TODO: doc

-type start_arg() :: {inet:socket(), gen_cop:codec(), gen_cop:handler_specs(), gen_cop:start_opts()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @see gen_cop:start/4
-spec start(start_arg()) -> {ok, pid()} | {error, gen_cop:start_err()}.
start(StartArg = {Socket, _, _, Options}) ->
    Name = proplists:get_value(name, Options),
    case Name =:= undefined orelse where(Name) of
        Pid when is_pid(Pid) -> {error, {already_started, Pid}};
        _                    ->
            ok = inet:setopts(Socket, [{active, false}, binary]),
            Ref = make_ref(),

            Pid = spawn_session(Ref, StartArg),
            _ = proplists:get_value(ack_fun, Options) =:= undefined andalso
                begin
                    ok = gen_tcp:controlling_process(Socket, Pid),
                    Pid ! Ref
                end,

            case proplists:get_value(async, Options, false) of
                true  -> {ok, Pid};
                false -> wait_start_result(Pid, Ref, Options)
            end
    end.

send(Data, State) ->
    State#state{send_queue = [Data | State#state.send_queue]}.

get_socket(#state{socket = Conn}) ->
    Conn.

add_handler(Pos, Mod, Arg, Context) ->
    Context#state{request_queue = [{add_handler, Pos, Mod, Arg} | Context#state.request_queue]}.

remove_handler(Reason, Context) ->
    Context#state{request_queue = [{remove_handler, Reason} | Context#state.request_queue]}.

-define(LOCATION, [{module, ?MODULE}, {line, ?LINE}, {pid, self()}]).

-define(MAKE_EX_REASON(ExClass, ExReason, MFArgs),
        {'EXIT', [{exception, ExClass, ExReason},
                  {trace, erlang:get_stacktrace()},
                  {location, ?LOCATION},
                  {mfargs, MFArgs}]}).

-define(FUNCALL_ERROR(Reason, MFArgs),
        {funcall_error, Reason, [{mfargs, MFArgs}, {location, ?LOCATION}]}).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec spawn_session(reference(), start_arg()) -> pid().
spawn_session(Ref, StartArg = {_, _, _, Options}) ->
    Name = proplists:get_value(name, Options),
    Parent = self(),
    AckFun = proplists:get_value(ack_fun, Options, fun () -> receive Ref -> ok end end),
    SyncFun =
        case proplists:get_value(async, Options, false) of
            true  -> fun (_) -> ok end;
            false -> fun (Result) -> Parent ! {Ref, Result}, receive Ref -> ok end end
        end,
    SpawnOptions0 = proplists:get_value(spawn_opt, Options, []),
    SpawnOptions1 =
        case proplists:get_value(link, Options, false) of
            false -> SpawnOptions0;
            true  -> [link | SpawnOptions0]
        end,
    proc_lib:spawn_opt(?MODULE, init, [Parent, Name, AckFun, SyncFun, StartArg], SpawnOptions1).

-spec wait_start_result(pid(), reference(), gen_cop:start_opts()) -> {ok, pid()} | {error, Reason} when Reason :: timeout | term().
wait_start_result(Pid, Ref, Options) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    Monitor = monitor(process, Pid),
    receive
        {Ref, Result} ->
            _ = demonitor(Monitor, [flush]),
            _ = Pid ! Ref,
            Result;
        {'DOWN', Monitor, _, _, Reason} ->
            {error, Reason}
    after Timeout ->
            _ = unlink(Pid),
            _ = exit(Pid, kill),
            _ = receive {'EXIT', Pid, _} -> ok after 0 -> ok end,
            _ = demonitor(Monitor, [flush]),
            {error, timeout}
    end.

-spec init(pid(), gen_cop:otp_name() | undefined, gen_cop:ack_fun(), sync_fun(), start_arg()) -> no_return().
init(Parent, Name, AckFun, SyncFun, StartArg) when Name =/= undefined ->
    case name_register(Name) of
        {false, Pid} -> SyncFun({error, {already_started, Pid}});
        true         -> init(Parent, undefined, AckFun, SyncFun, StartArg)
    end;
init(Parent, _, AckFun, SyncFun, {Socket, Codec, HandlerSpecs, Options}) ->
    _ = AckFun(),
    State0 =
        #state{
           socket = Socket,
           codec  = Codec
          },
    case init_handlers(lists:reverse(HandlerSpecs), State0) of
        {error, Reason, State1} ->
            _ = SyncFun({error, Reason}),
            terminate(Reason, State1);
        {ok, State1}            ->
            _  = SyncFun({ok, self()}),
            ok = inet:setopts(Socket, [{active, once}]),
            loop(State1, Parent, sys:debug_options(proplists:get_value(debug, Options, [])))
    end.

loop(State0, Parent, Debug) ->
    %% TODO: exception handling (if need)
    Result =
        receive
            {tcp, _, Bin} ->
                handle_recv(Bin, State0);
            {call, Request, From} -> % XXX:
                handle_call(Request, From, State0);
            {cast, Request} -> % XXX:
                handle_cast(Request, State0);
            {send, Data} ->
                handle_send(Data, State0);

            {tcp_closed, _}           -> {error, {shutdown, tcp_closed}, State0};
            {tcp_error, _, TcpReason} -> {error, {shutdown, {tcp_error, TcpReason}}, State0};

            %% sys releated messages
            {system, _, _} = SystemMessage -> SystemMessage;
            %% TODO: {From, Tag, get_modules} -> _ = ?SYSTEM_REPLY(From, Tag, get_modules(State)), State;

            Msg ->
                handle_info(Msg, State0)
        end,
    case Result of
        {ok, State1}              -> loop(State1, Parent, Debug);
        {error, Reason, State1}   -> terminate(Reason, State1);
        {system, From2, Request2} -> sys:handle_system_msg(Request2, From2, Parent, ?MODULE, Debug, State0)
    end.

handle_send(Data, State0) ->
    %% TOOD: can or cann't send flag
    flush_send_queue(State0#state{send_queue = [Data | State0#state.send_queue]}).

flush_send_queue(State0 = #state{send_queue = Queue}) ->
    %% TODO: exception handling
    DataList = lists:reverse(Queue),
    case gen_cop_codec:encode(DataList, State0#state.codec) of
        {error, Reason, Codec} -> {error, Reason, State0#state{codec = Codec, send_queue = []}};
        {ok, IoData, Codec}    ->
            State1 = State0#state{codec = Codec, send_queue = []},
            case gen_tcp:send(State1#state.socket, IoData) of
                {error, Reason} -> {error, Reason, State1};
                ok              -> {ok, State1}
            end
    end.

handle_recv(Bin, State0) ->
    case gen_cop_codec:decode(Bin, State0#state.codec) of
        {error, Reason, Codec} -> {error, Reason, State0#state{codec = Codec}};
        {ok, Messages, Codec}  ->
            State1 = State0#state{codec = Codec},
            case handle_messages(Messages, State1) of
                {error, Reason, State2} -> {error, Reason, State2};
                {ok, State2} ->
                    ok = inet:setopts(State2#state.socket, [{active, once}]), % TODO: error handling
                    {ok, State2}
            end
    end.

handle_messages([], State) ->
    {ok, State};
handle_messages([Msg | Messages], State0) ->
    case handle_message(Msg, State0) of
        {error, Reason, State1} -> {error, Reason, State1};
        {ok, State1}            -> handle_messages(Messages, State1)
    end.

handle_message(Msg, State = #state{handlers = []}) ->
    %% TODO: error message
    {error, {unhandled_message, Msg}, fix_handlers(State)};
handle_message(Msg, State0 = #state{handlers = [Handler0 | Handlers]}) ->
    case handler_handle_data(Msg, Handler0, State0#state{handlers = Handlers}) of
        {stop, Reason, State1} -> {error, Reason, fix_handlers(State1)};
        {delegate, State1}     -> handle_message(Msg, State1);
        {ok, State1}           -> {ok, fix_handlers(State1)}
    end.

handle_call(Request, From, State = #state{handlers = []}) ->
    ok = gen_cop:reply(From, {error, unhandled_request}), % XXX: reason
    {error, {unhandled_call, Request, From}, fix_handlers(State)};
handle_call(Request, From, State0 = #state{handlers = [Handler0 | Handlers]}) ->
    case handler_handle_call(Request, From, Handler0, State0#state{handlers = Handlers}) of
        {stop, Reason, State1} -> {error, Reason, fix_handlers(State1)};
        {delegate, State1}     -> handle_call(Request, From, State1);
        {ok, State1}           -> {ok, fix_handlers(State1)}
    end.

handle_cast(Request, State = #state{handlers = []}) ->
    %% XXX:
    {error, {unhandled_cast, Request}, fix_handlers(State)};
handle_cast(Request, State0 = #state{handlers = [Handler0 | Handlers]}) ->
    case handler_handle_cast(Request, Handler0, State0#state{handlers = Handlers}) of
        {stop, Reason, State1} -> {error, Reason, fix_handlers(State1)};
        {delegate, State1}     -> handle_cast(Request, State1);
        {ok, State1}           -> {ok, fix_handlers(State1)}
    end.

handler_handle_cast(Request, {HMod, HState0}, State0) ->
    try
        case HMod:handle_cast(Request, HState0, State0) of
            {stop, Reason, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                process_queue_on_error({HMod, handle_cast, [Request, HState0, State0]}, Reason, State2);
            {delegate, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                case process_queue(State2) of
                    {error, Reason, State3} -> {stop, Reason, State3};
                    {ok, State3}            -> {delegate, State3}
                end;
            {noreply, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                process_queue(State2)
        end
    catch
        ExClass:ExReason ->
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {HMod, handle_cast, [Request, HState0, State0]}), State0}
    end.

%% TODO: Request => Info
handle_info(Request, State = #state{handlers = []}) ->
    %% XXX:
    {error, {unhandled_info, Request}, fix_handlers(State)};
handle_info(Request, State0 = #state{handlers = [Handler0 | Handlers]}) ->
    case handler_handle_info(Request, Handler0, State0#state{handlers = Handlers}) of
        {stop, Reason, State1} -> {error, Reason, fix_handlers(State1)};
        {delegate, State1}     -> handle_info(Request, State1);
        {ok, State1}           -> {ok, fix_handlers(State1)}
    end.

handler_handle_info(Request, {HMod, HState0}, State0) ->
    try
        case HMod:handle_info(Request, HState0, State0) of
            {stop, Reason, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                process_queue_on_error({HMod, handle_info, [Request, HState0, State0]}, Reason, State2);
            {delegate, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                case process_queue(State2) of
                    {error, Reason, State3} -> {stop, Reason, State3};
                    {ok, State3}            -> {delegate, State3}
                end;
            {noreply, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                process_queue(State2)
        end
    catch
        ExClass:ExReason ->
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {HMod, handle_info, [Request, HState0, State0]}), State0}
    end.


handler_handle_call(Request, From, {HMod, HState0}, State0) ->
    try
        case HMod:handle_call(Request, From, HState0, State0) of
            {stop, Reason, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                process_queue_on_error({HMod, handle_call, [Request, From, HState0, State0]}, Reason, State2);
            {stop, Reply, Reason, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                ok = gen_cop:reply(From, Reply),
                process_queue_on_error({HMod, handle_call, [Request, From, HState0, State0]}, Reason, State2);
            {delegate, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                case process_queue(State2) of
                    {error, Reason, State3} -> {stop, Reason, State3};
                    {ok, State3}            -> {delegate, State3}
                end;
            {noreply, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                process_queue(State2);
            {reply, Reply, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                ok = gen_cop:reply(From, Reply),
                process_queue(State2)
        end
    catch
        ExClass:ExReason ->
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {HMod, handle_call, [Request, From, HState0, State0]}), State0}
    end.

handler_handle_data(Msg, {HMod, HState0}, State0) ->
    try
        case HMod:handle_data(Msg, HState0, State0) of
            {stop, Reason, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                process_queue_on_error({HMod, handle_data, [Msg, HState0, State0]}, Reason, State2);
            {delegate, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                case process_queue(State2) of
                    {error, Reason, State3} -> {stop, Reason, State3};
                    {ok, State3}            -> {delegate, State3}
                end;
            {noreply, HState1, State1} ->
                State2 = State1#state{done_handlers = [{HMod, HState1} | State1#state.done_handlers]},
                process_queue(State2)
        end
    catch
        ExClass:ExReason ->
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {HMod, handle_data, [Msg, HState0, State0]}), State0}
    end.

%% XXX: name
fix_handlers(State) ->
    State#state{handlers = lists:reverse(State#state.done_handlers, State#state.handlers), done_handlers = []}.

init_handlers([], State0) ->
    {ok, State0};
init_handlers([{Mod, Arg} | HandlerSpecs], State0) ->
    case handler_init(Mod, Arg, State0) of
        {error, Reason, State1} -> {error, Reason, State1};
        {ok, State1}            -> init_handlers(HandlerSpecs, State1)
    end.

handler_init(Mod, Arg, State0) ->
    try
        case Mod:init(Arg, State0) of
            {stop, Reason, State1}     -> process_queue_on_error({Mod, init, [Arg, State0]}, Reason, State1);
            {ok, HandlerState, State1} ->
                State2 = State1#state{handlers = [{Mod, HandlerState} | State1#state.handlers]},
                process_queue(State2)
        end
    catch
        ExClass:ExReason ->
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {Mod, init, [Arg, State0]}), State0}
    end.

terminate(Reason, State0) ->
    ok = terminate_handlers(Reason, State0),
    exit(Reason).

terminate_handlers(_Reason, #state{handlers = []}) ->
    ok;
terminate_handlers(Reason, State0 = #state{handlers = [Handler | Handlers]}) ->
    State1 = State0#state{handlers = Handlers},
    State2 = handler_terminate(Reason, Handler, State1),
    terminate_handlers(Reason, State2).

handler_terminate(Reason, {Mod, HandlerState}, State0) ->
    try
        State1 = Mod:terminate(Reason, HandlerState, State0),
        State2 = process_queue_on_terminate({Mod, terminate, [Reason, HandlerState, State0]}, State1),
        State2
    catch
        ExClass:ExReason ->
            %% TODO: error message or abort
            {error, ?MAKE_EX_REASON(ExClass, ExReason, {Mod, terminate, [Reason, HandlerState, State0]}), State0}
    end.

process_queue_on_error(MFArgs, Reason0, State0) ->
    case process_queue(State0) of
        {error, Reason1, State1} ->
            _ = error_logger:warning_report(
                  [{module, ?MODULE}, {line, ?LINE}, {pid, self()}, {overwrote_error, Reason0}, {handler_mfargs, MFArgs}]),
            {error, Reason1, State1};
        {ok, State1} ->
            {error, Reason0, State1}
    end.

process_queue_on_terminate(MFArgs, State0) ->
    case process_queue(State0) of
        {error, Reason, State1} ->
            _ = error_logger:warning_report(
                  [{module, ?MODULE}, {line, ?LINE}, {pid, self()}, {dropped_error, Reason}, {handler_mfargs, MFArgs}]),
            State1;
        {ok, State1} ->
                State1
        end.

process_queue(State0) ->
    case flush_send_queue(State0) of
        {error, Reason, State1} -> {error, Reason, State1};
        {ok, State1}            -> process_request_queue(State1)
    end.

process_request_queue(State = #state{request_queue = []}) ->
    {ok, State};
process_request_queue(State) ->
    process_request_queue(lists:reverse(State#state.request_queue), State#state{request_queue = []}).

process_request_queue([], State) ->
    {ok, State}; %process_request_queue(State);
process_request_queue([X | Queue], State0) ->
    case process_request(X, State0) of
        {error, Reason, State1} ->
            State2 = State1#state{request_queue = lists:reverse(Queue, State1#state.request_queue)},
            {error, Reason, State2};
        {ok, State1} ->
            process_request_queue(Queue, State1)
    end.

process_request({remove_handler, Reason}, State0 = #state{done_handlers = [Handler | Handlers]}) ->
    %% TODO: duplicate remove check
    State1 = handler_terminate(Reason, Handler, State0#state{done_handlers = Handlers}),
    {ok, State1};
%% process_request({swap_handler, Reason, NewHMode, NewHArg}, State0 = #state{done_handlers = [Handler | Handlers]}) ->
%% swap = remove + add
process_request({add_handler, Pos, HMod, HArg}, State0 = #state{done_handlers = PreHandlers, handlers = PostHandlers}) ->
    case handler_init(HMod, HArg, State0) of
        {error, Reason, State1}                         -> {error, Reason, State1};
        {ok, State1 = #state{handlers = [Handler | _]}} -> % XXX:
            case Pos of
                front -> {ok, State1#state{done_handlers = PreHandlers ++ [Handler], handlers = PostHandlers}};
                back  -> {ok, State1#state{done_handlers = PreHandlers, handlers = PostHandlers ++ [Handler]}};
                pre   -> {ok, State1#state{done_handlers = [Handler | PreHandlers], handlers = PostHandlers}};
                post  -> {ok, State1#state{done_handlers = PreHandlers, handlers = [Handler | PostHandlers]}}
            end
    end.


where({global, Name}) -> global:whereis_name(Name);
where({via, Module, Name}) -> Module:whereis_name(Name);
where({local, Name})  -> whereis(Name).

name_register({local, Name} = LN) ->
    try register(Name, self()) of
        true -> true
    catch
        error:_ ->
            {false, where(LN)}
    end;
name_register({global, Name} = GN) ->
    case global:register_name(Name, self()) of
        yes -> true;
        no -> {false, where(GN)}
    end;
name_register({via, Module, Name} = GN) ->
    case Module:register_name(Name, self()) of
        yes ->
            true;
        no ->
            {false, where(GN)}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'sys' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
system_continue(Parent, Debug, State) ->
    loop(State, Parent, Debug).

%% @private
%%-spec system_terminate(any(), pid(), any(), session()) -> no_return(). % dialyzerの警告抑制のためのspec
system_terminate(Reason, _Parent, _Debug, State) ->
    terminate(Reason, State).

%% @private
system_code_change(State, Module, OldVsn, Extra) ->
    Handlers = [case Module =:= HMod of
                    true ->
                        {ok, HState1} = HMod:codec_hange(OldVsn, HState0, Extra),
                        {HMod, HState1};
                    false ->
                        {HMod, HState0}
                end || {HMod, HState0} <- State#state.handlers],
    {ok, State#state{handlers = Handlers}}.
