-module(gen_cop_handler).

-export_type([arg/0, state/0, spec/0]).

-type arg() :: term().
-type state() :: term().

-type spec() :: {module(), arg()}.

-callback init(arg(), gen_cop:context()) ->
    {ok, state(), gen_cop:context()} |
    {stop, Reason::term(), gen_cop:context()}.

-callback handle_data(gen_cop:data(), state(), gen_cop:context()) ->
    {noreply, state(), gen_cop:context()} |
    {delegate, state(), gen_cop:context()} |
    {stop, Reason::term(), state(), gen_cop:context()}.

-callback handle_call(gen_cop:request(), gen_cop:from(), state(), gen_cop:context()) ->
    {reply, gen_cop:reply(), state(), gen_cop:context()} |
    {noreply, state(), gen_cop:context()} |
    {delegate, state(), gen_cop:context()} |
    {stop, Reason::term(), state(), gen_cop:context()} |
    {stop, gen_cop:reply(), Reason::term(), state(), gen_cop:context()}.

-callback handle_cast(gen_cop:request(), state(), gen_cop:context()) ->
    {noreply, state(), gen_cop:context()} |
    {delegate, state(), gen_cop:context()} |
    {stop, Reason::term(), state(), gen_cop:context()}.

-callback handle_info(gen_cop:info(), state(), gen_cop:context()) ->
    {noreply, state(), gen_cop:context()} |
    {delegate, state(), gen_cop:context()} |
    {stop, Reason::term(), state(), gen_cop:context()}.

-callback terminate(Reason::term(), state(), gen_cop:context()) ->
    gen_cop:context().

-callback code_change(OldVsn::term(), state(), Extra::term()) ->
    {ok, state()} |
    {error, Reason::term()}.

%% gen_cop:delegate().
%% gen_cop:swap().
%% gen_cop:remove().
%% gen_cop:add().
