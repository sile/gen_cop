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
-export([encode/2, decode/2]).

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

-type codec_module() :: module().
-type codec_state() :: term().

-type encode_fun() :: fun (([gen_cop:message()], codec_state()) -> encode_result()).
-type decode_fun() :: fun ((binary(), codec_state()) -> decode_result()).

-type encode_result() :: {ok, iodata(), codec_state()} | {error, Reason::term(), codec_state()}.
-type decode_result() :: {ok, [gen_cop:message()], codec_state()} | {error, Reason::term(), codec_state()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Callback API
%%----------------------------------------------------------------------------------------------------------------------
-callback encode([gen_cop:message()], codec_state()) -> encode_result().
-callback decode(binary(), codec_state()) -> decode_result().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec make(codec_module(), codec_state()) -> codec().
make(Module, State) ->
    make(State, fun Module:encode/2, fun Module:decode/2).

-spec make(codec_state(), encode_fun(), decode_fun()) -> codec().
make(State, EncodeFun, DecodeFun) ->
    #?CODEC{state = State, encode = EncodeFun, decode = DecodeFun}.

-spec encode([gen_cop:message()], codec()) -> encode_result().
encode(Messages, Codec) ->
    case (Codec#?CODEC.encode)(Messages, Codec#?CODEC.state) of
        {ok, Encoded, State}   -> {ok, Encoded, Codec#?CODEC{state = State}};
        {error, Reason, State} -> {error, Reason, Codec#?CODEC{state = State}}
    end.

-spec decode(binary(), codec_state()) -> decode_result().
decode(Bin, Codec) ->
    case (Codec#?CODEC.decode)(Bin, Codec#?CODEC.state) of
        {ok, Messages, State}  -> {ok, Messages, Codec#?CODEC{state = State}};
        {error, Reason, State} -> {error, Reason, Codec#?CODEC{state = State}}
    end.
