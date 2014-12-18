%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Handler Interface
%%
%% TODO: doc
-module(gen_cop_handler).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/2, handle_data/3, handle_call/4, handle_cast/3, handle_info/3, terminate/3, code_change/3]).

-export_type([spec/0]).
-export_type([arg/0, callback_opt/0, callback_opts/0]).
-export_type([handler/0]).
-epxort_type([state/0]).

-export_type([init_fun/0]).
-export_type([handle_data_fun/0]).
-export_type([handle_call_fun/0]).
-export_type([handle_cast_fun/0]).
-export_type([handle_info_fun/0]).
-export_type([terminate_fun/0]).
-export_type([code_change_fun/0]).

-export_type([handle_result/0, handle_result/1]).
-export_type([context/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(HANDLE(Context, Handler, FunName, Args),
        try
            case (Handler#?HANDLER.callback#callback.FunName)Args of
                {stop, __Reason, __State, __Context} -> {stop, __Reason, Handler#?HANDLER{state = __State}, __Context};
                {ok, __State, __Context}             -> {ok, Handler#?HANDLER{state = __State}, __Context}
            end
        catch
            ExClass:ExReason ->
                {stop, {'EXIT', {ExClass, ExReason, erlang:get_stacktrace()}}, Handler, Context}
        end).

-define(HANDLER, ?MODULE).

-record(callback,
        {
          init        :: init_fun(),
          handle_data :: handle_data_fun(),
          handle_call :: handle_call_fun(),
          handle_cast :: handle_cast_fun(),
          handle_info :: handle_info_fun(),
          terminate   :: terminate_fun(),
          code_change :: code_change_fun()
        }).

-record(?HANDLER,
        {
          callback :: #callback{},
          state    :: state()
        }).

-type spec() :: {module(), arg()} | {module(), arg(), callback_opts()}.
-type arg() :: term().

-opaque handler() :: #?HANDLER{}.
-type state() :: term().

-type callback_opts() :: [callback_opt()].
-type callback_opt()  :: {init, init_fun()}
                       | {handle_data, handle_data_fun()}
                       | {handle_call, handle_call_fun()}
                       | {handle_cast, handle_cast_fun()}
                       | {handle_info, handle_info_fun()}
                       | {terminate, terminate_fun()}
                       | {code_change, code_change_fun()}.

-type init_fun()        :: fun ((arg(), context()) -> {ok, state(), context()} | {stop, Reason::term(), context()}).
-type handle_data_fun() :: fun ((gen_cop:data(), state(), context()) -> handle_result()).
-type handle_call_fun() :: fun ((gen_cop:request(), gen_cop:from(), state(), context()) -> handle_result()).
-type handle_cast_fun() :: fun ((gen_cop:request(), state(), context()) -> handle_result()).
-type handle_info_fun() :: fun ((gen_cop:info(), state(), context()) -> handle_result()).
-type terminate_fun()   :: fun ((Reason::term(), state(), context()) -> context()).
-type code_change_fun() :: fun ((OldVsn::term(), state(), Extra::term()) -> {ok, state()} | {error, Reason::term()}).

-type handle_result(State) :: {ok, State, context()} | {stop, Reason::term(), State, context()}.
-type handle_result()      :: handle_result(state()).

-type context() :: gen_cop:context().

%%----------------------------------------------------------------------------------------------------------------------
%% Callback API
%%----------------------------------------------------------------------------------------------------------------------
-callback init(arg(), context()) -> {ok, state(), context()} | {stop, Reason::term(), context()}.
-callback handle_data(gen_cop:data(), state(), context()) -> handle_result().
-callback handle_call(gen_cop:request(), gen_cop:from(), state(), context()) -> handle_result().
-callback handle_cast(gen_cop:request(), state(), context()) -> handle_result().
-callback handle_info(gen_cop:info(), state(), context()) -> handle_result().
-callback terminate(Reason::term(), state(), context()) -> context().
-callback code_change(OldVsn::term(), state(), Extra::term()) -> {ok, state()} | {error, Reason::term()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec init(spec(), context()) -> {ok, handler(), context()} | {stop, Reason::term(), context()}.
init({Module, Arg}, Context) ->
    init({Module, Arg, []}, Context);
init({Module, Arg, CallbackOptions}, Context0) ->
    Callback =
        #callback{
           init        = proplists:get_value(init,        CallbackOptions, fun Module:init/2),
           handle_data = proplists:get_value(handle_data, CallbackOptions, fun Module:handle_data/3),
           handle_call = proplists:get_value(handle_call, CallbackOptions, fun Module:handle_call/4),
           handle_cast = proplists:get_value(handle_cast, CallbackOptions, fun Module:handle_cast/3),
           handle_info = proplists:get_value(handle_info, CallbackOptions, fun Module:handle_info/3),
           terminate   = proplists:get_value(terminate,   CallbackOptions, fun Module:terminate/3),
           code_change = proplists:get_value(code_change, CallbackOptions, fun Module:code_change/3)
          },
    try
        case (Callback#callback.init)(Arg, Context0) of
            {stop, Reason, Context1} -> {stop, Reason, Context1};
            {ok, State, Context1}    ->
                Handler = #?HANDLER{callback = Callback, state = State},
               {ok, Handler, Context1}
        end
    catch
        ExClass:ExReason ->
            {stop, {'EXIT', {ExClass, ExReason, erlang:get_stacktrace()}}, Context0}
    end.

-spec handle_data(gen_cop:data(), handler(), context()) -> handle_result(handler()).
handle_data(Data, Handler, Context) ->
    ?HANDLE(Context, Handler, handle_data, (Data, Handler#?HANDLER.state, Context)).

-spec handle_call(gen_cop:request(), gen_cop:from(), handler(), context()) -> handle_result(handler()).
handle_call(Request, From, Handler, Context) ->
    ?HANDLE(Context, Handler, handle_call, (Request, From, Handler#?HANDLER.state, Context)).

-spec handle_cast(gen_cop:request(), handler(), context()) -> handle_result(handler()).
handle_cast(Request, Handler, Context) ->
    ?HANDLE(Context, Handler, handle_cast, (Request, Handler#?HANDLER.state, Context)).

-spec handle_info(gen_cop:info(), handler(), context()) -> handle_result(handler()).
handle_info(Info, Handler, Context) ->
    ?HANDLE(Context, Handler, handle_info, (Info, Handler#?HANDLER.state, Context)).

-spec terminate(term(), handler(), context()) -> context().
terminate(Reason, Handler, Context) ->
    try
        (Handler#?HANDLER.callback#callback.terminate)(Reason, Handler, Context)
    catch
        ExClass:ExReason ->
            _ = error_logger:error_report(
                  [
                   {module, ?MODULE},
                   {function, terminate},
                   {line, ?LINE},
                   {pid, self()},
                   {exception, {ExClass, ExReason, erlang:get_stacktrace()}},
                   {args, [Reason, Handler, Context]}
                  ]),
            Context
    end.

-spec code_change(term(), handler(), term()) -> {ok, handler()} | {error, Reason::term()}.
code_change(OldVsn, Handler, Extra) ->
    try
        case (Handler#?HANDLER.callback#callback.code_change)(OldVsn, Handler#?HANDLER.state, Extra) of
            {error, Reason} -> {error, Reason};
            {ok, State}     -> {ok, Handler#?HANDLER{state = State}}
        end
    catch
        ExClass:ExReason ->
            {error, {'EXIT', {ExClass, ExReason, erlang:get_stacktrace()}}}
    end.
