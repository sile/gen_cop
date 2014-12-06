-module(gen_cop_codec).

-export_type([codec/0]).

-type codec() :: term().

-callback encode([gen_cop:data()], codec()) -> {ok, iodata(), codec()} | {error, Reason::term(), codec()}.
-callback decode(binary(), codec()) -> {ok, [gen_cop:data()], codec()} | {error, Reason::term(), codec()}.
