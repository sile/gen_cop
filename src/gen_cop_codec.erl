%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Encoder/Decoder Interface
%%
%% TODO: doc
-module(gen_cop_codec).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([make/2, make/3]).
-export([get_state/1, set_state/2]).
-export([encode/3, decode/3]).

-export_type([codec/0]).
-export_type([codec_module/0, codec_state/0]).
-export_type([encode_fun/0, decode_fun/0]).
-export_type([encode_result/0, decode_result/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(CODEC, ?MODULE).

-record(?CODEC,
        {
          state  :: codec_state(),
          encode :: encode_fun(),
          decode :: decode_fun()
        }).

-opaque codec() :: #?CODEC{}.

-type context() :: gen_cop_context:context().

-type codec_module() :: module().
-type codec_state() :: term().

-type encode_fun() :: fun (([gen_cop:message()], codec_state(), context()) -> encode_result()).
-type decode_fun() :: fun ((binary(), codec_state(), context()) -> decode_result()).

-type encode_result() :: {ok, iodata(), codec_state(), context()} | {error, Reason::term(), codec_state(), context()}.
-type decode_result() :: {ok, [gen_cop:message()], codec_state(), context()} | {error, Reason::term(), codec_state(), context()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Callback API
%%----------------------------------------------------------------------------------------------------------------------
-callback encode([gen_cop:message()], codec_state(), context()) -> encode_result().
-callback decode(binary(), codec_state(), context()) -> decode_result().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec make(codec_module(), codec_state()) -> codec().
make(Module, State) ->
    make(State, fun Module:encode/3, fun Module:decode/3).

-spec get_state(codec()) -> codec_state().
get_state(#?CODEC{state = State}) ->
    State.

-spec set_state(codec_state(), codec()) -> codec().
set_state(State, Codec) ->
    Codec#?CODEC{state = State}.

-spec make(codec_state(), encode_fun(), decode_fun()) -> codec().
make(State, EncodeFun, DecodeFun) ->
    #?CODEC{state = State, encode = EncodeFun, decode = DecodeFun}.

-spec encode([gen_cop:message()], codec(), context()) -> encode_result().
encode(Messages, Codec, Context0) ->
    case (Codec#?CODEC.encode)(Messages, Codec#?CODEC.state, Context0) of
        {ok, Encoded, State, Context1}   -> {ok, Encoded, Codec#?CODEC{state = State}, Context1};
        {error, Reason, State, Context1} -> {error, Reason, Codec#?CODEC{state = State}, Context1}
    end.

-spec decode(binary(), codec_state(), context()) -> decode_result().
decode(Bin, Codec, Context0) ->
    case (Codec#?CODEC.decode)(Bin, Codec#?CODEC.state, Context0) of
        {ok, Messages, State, Context1}  -> {ok, Messages, Codec#?CODEC{state = State}, Context1};
        {error, Reason, State, Context1} -> {error, Reason, Codec#?CODEC{state = State}, Context1}
    end.
