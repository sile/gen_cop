%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Handler Interface
%%
%% TODO: doc
-module(gen_cop_handler).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([make_instance/1]).
-export([get_id/1]).
-export([init/2, handle_data/3, handle_call/4, handle_cast/3, handle_info/3, terminate/3, code_change/4]).

-export_type([spec/0]).
-export_type([id/0]).
-export_type([arg/0, callback_opt/0, callback_opts/0]).
-export_type([spec_opt/0, spec_opts/0]).
-export_type([handler/0, uninitialized_handler/0, initialized_handler/0]).
-export_type([header/0]).
-epxort_type([state/0]).

-export_type([init_fun/0]).
-export_type([handle_data_fun/0]).
-export_type([handle_call_fun/0]).
-export_type([handle_cast_fun/0]).
-export_type([handle_info_fun/0]).
-export_type([terminate_fun/0]).
-export_type([code_change_fun/0]).

-export_type([handle_result/0]).
-export_type([context/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(FUNCALL(Context, Header, FunName, Args),
        try
            (Header#?HEADER.FunName)Args
        catch
            ExClass:ExReason -> gen_cop_context:raise(ExClass, ExReason, Context)
        end).

-define(HEADER, ?MODULE).

-record(?HEADER,
        {
          id          :: id(),
          module      :: module(),
          init        :: init_fun(),
          handle_data :: handle_data_fun(),
          handle_call :: handle_call_fun(),
          handle_cast :: handle_cast_fun(),
          handle_info :: handle_info_fun(),
          terminate   :: terminate_fun(),
          code_change :: code_change_fun()
        }).

-type spec() :: {module(), arg()}
              | {module(), arg(), spec_opts()}.

-type id() :: term().
-type arg() :: term().
-type state() :: term().

-type handler() :: uninitialized_handler() | initialized_handler().
-type uninitialized_handler() :: {header(), arg()}.
-type initialized_handler() :: {header(), state()}.
-opaque header() :: #?HEADER{}.

-type spec_opts() :: [spec_opt()].
-type spec_opt() :: {id, id()}
                  | {callback, callback_opts()}.

-type callback_opts() :: [callback_opt()].
-type callback_opt()  :: {init, init_fun()}
                       | {handle_data, handle_data_fun()}
                       | {handle_call, handle_call_fun()}
                       | {handle_cast, handle_cast_fun()}
                       | {handle_info, handle_info_fun()}
                       | {terminate, terminate_fun()}
                       | {code_change, code_change_fun()}.

-type init_fun()        :: fun ((arg(), context())                                      -> handle_result()).
-type handle_data_fun() :: fun ((gen_cop:data(), state(), context())                    -> handle_result()).
-type handle_call_fun() :: fun ((gen_cop:request(), gen_cop:from(), state(), context()) -> handle_result()).
-type handle_cast_fun() :: fun ((gen_cop:request(), state(), context())                 -> handle_result()).
-type handle_info_fun() :: fun ((gen_cop:info(), state(), context())                    -> handle_result()).
-type terminate_fun()   :: fun ((Reason::term(), state(), context())                    -> handle_result()).
-type code_change_fun() :: fun ((OldVsn::term(), state(), Extra::term(), context())     -> handle_result()).

-type handle_result() :: gen_cop_context:handler_result().

-type context() :: gen_cop:context().

%%----------------------------------------------------------------------------------------------------------------------
%% Callback API
%%----------------------------------------------------------------------------------------------------------------------
-callback init(arg(), context())                                             -> handle_result().
-callback handle_data(gen_cop:data(), state(), context())                    -> handle_result().
-callback handle_call(gen_cop:request(), gen_cop:from(), state(), context()) -> handle_result().
-callback handle_cast(gen_cop:request(), state(), context())                 -> handle_result().
-callback handle_info(gen_cop:info(), state(), context())                    -> handle_result().
-callback terminate(Reason::term(), state(), context())                      -> handle_result().
-callback code_change(OldVsn::term(), state(), Extra::term(), context())     -> handle_result().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec make_instance(spec()) -> uninitialized_handler().
make_instance({Module, Arg}) ->
    %% TODO: validate
    make_instance({Module, Arg, []});
make_instance({Module, Arg, Options}) ->
    CallbackOptions = proplists:get_value(callback, Options, []),
    Header =
        #?HEADER{
            id          = proplists:get_value(id, Options, Module),
            module      = Module,
            init        = proplists:get_value(init,        CallbackOptions, fun Module:init/2),
            handle_data = proplists:get_value(handle_data, CallbackOptions, fun Module:handle_data/3),
            handle_call = proplists:get_value(handle_call, CallbackOptions, fun Module:handle_call/4),
            handle_cast = proplists:get_value(handle_cast, CallbackOptions, fun Module:handle_cast/3),
            handle_info = proplists:get_value(handle_info, CallbackOptions, fun Module:handle_info/3),
            terminate   = proplists:get_value(terminate,   CallbackOptions, fun Module:terminate/3),
            code_change = proplists:get_value(code_change, CallbackOptions, fun Module:code_change/4)
           },
    {Header, Arg}.

-spec get_id(handler()) -> id().
get_id({#?HEADER{id = Id}, _}) -> Id.

%% TODO: move to gen_cop_context
-spec init(uninitialized_handler(), context()) -> gen_cop_context:handler_result().
init({Header, Arg}, Context) ->
    ?FUNCALL(Context, Header, init, (Arg, Context)).

-spec handle_data(gen_cop:data(), initialized_handler(), context()) -> gen_cop_context:handler_result().
handle_data(Data, {Header, State}, Context) ->
    ?FUNCALL(Context, Header, handle_data, (Data, State, Context)).

-spec handle_call(gen_cop:request(), gen_cop:from(), handler(), context()) -> gen_cop_context:handler_result().
handle_call(Request, From, {Header, State}, Context) ->
    ?FUNCALL(Context, Header, handle_call, (Request, From, State, Context)).

-spec handle_cast(gen_cop:request(), handler(), context()) -> gen_cop_context:handler_result().
handle_cast(Request, {Header, State}, Context) ->
    ?FUNCALL(Context, Header, handle_cast, (Request, State, Context)).

-spec handle_info(gen_cop:info(), handler(), context()) -> gen_cop_context:handler_result().
handle_info(Info, {Header, State}, Context) ->
    ?FUNCALL(Context, Header, handle_info, (Info, State, Context)).

-spec terminate(term(), handler(), context()) -> gen_cop_context:handler_result().
terminate(Reason, {Header, State}, Context) ->
    ?FUNCALL(Context, Header, terminate, (Reason, State, Context)).

-spec code_change(term(), handler(), term(), context()) -> handle_result().
code_change(OldVsn, {Header, State}, Extra, Context) ->
    ?FUNCALL(Context, Header, code_change, (OldVsn, State, Extra, Context)).
