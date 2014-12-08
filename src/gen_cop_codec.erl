-module(gen_cop_codec).

-export([encode/2, decode/2]).
-export_type([codec/0]).

-type codec() :: term().

-callback encode([gen_cop:data()], codec()) -> {ok, iodata(), codec()} | {error, Reason::term(), codec()}.
-callback decode(binary(), codec()) -> {ok, [gen_cop:data()], codec()} | {error, Reason::term(), codec()}.

encode(DataList, {Mod, State}) ->
    Result = {_, _, State2} = Mod:encode(DataList, State),
    setelement(3, Result, {Mod, State2}).

decode(Bin, {Mod, State}) ->
    Result = {_, _, State2} = Mod:decode(Bin, State),
    setelement(3, Result, {Mod, State2}).
