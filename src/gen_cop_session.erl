-module(gen_cop_session).

-export([start_link/4]).
-export([init/6]).

-record(state,
        {
          connection :: gen_cop_connection:connection(),
          codec :: gen_cop_codec:codec(),
          handlers = [] :: [gen_cop_handler:handler()]
        }).

-define(LOCATION, [{module, ?MODULE}, {line, ?LINE}, {pid, self()}]).

-define(MAKE_EX_REASON(ExClass, ExReason, MFArgs),
        {'EXIT', [{exception, ExClass, ExReason},
                  {trace, erlang:get_stacktrace()},
                  {location, ?LOCATION},
                  {mfargs, MFArgs}]}).

-define(FUNCALL_ERROR(Reason, MFArgs),
        {funcall_error, Reason, [{mfargs, MFArgs}, {location, ?LOCATION}]}).

-spec start_link(gen_cop_connection:connection(), gen_cop_codec:codec(), [gen_cop_handler:spec()], gen_cop:options()) ->
                        {ok, pid()} | {error, Reason} when
      Reason :: term().
start_link(Connection, Codec, HandlerSpecs, Options) ->
    Ref = make_ref(),
    Parent = self(),
    SyncFun = fun () -> receive Ref -> ok end end,
    AckFun = fun () -> Parent ! Ref, ok end,
    Pid = proc_lib:spawn_link(?MODULE, init, [SyncFun, AckFun, Connection, Codec, HandlerSpecs, Options]),
    Monitor = monitor(process, Pid),
    ok = gen_cop_connection:controlling_process(Connection, Pid),
    _ = Pid ! Ref,
    receive
        Ref -> {ok, Pid};
        {'DOWN', Monitor, _, _, Reason} ->
            {error, {'EXIT', Reason}}
    end.

init(SyncFun, AckFun, Connection, Codec, HandlerSpecs, _Options) ->
    ok = SyncFun(),
    State0 =
        #state{
           connection = Connection,
           codec = Codec
          },
    case init_handlers(lists:reverse(HandlerSpecs), State0) of % XXX: reverse
        {error, Reason, State1} -> terminate(Reason, State1);
        {ok, State1}            ->
            ok = AckFun(),
            case gen_cop_connection:setopts(Connection, [{active, true}]) of
                {error, Reason} -> terminate(?FUNCALL_ERROR(Reason, {gen_cop_connection, setopts, [Connection, [{active, true}]]}), State1);
                ok              -> loop(State1)
            end
    end.

loop(State0) ->
    receive
        _ ->
            loop(State0)
    end.

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

process_queue(State) ->
    %% TODO:
    {ok, State}.
