-module(gen_cop_handler).

-export_type([arg/0, state/0]).

-type arg() :: term().
-type state() :: term().

-callback init(arg(), gen_cop:context()) ->
    {ok, state(), gen_cop:context()} |
    {stop, Reason::term(), gen_cop:context()}.

-callback handle_data(gen_cop:data(), state(), gen_cop:context()) ->
    {noreply, state(), gen_cop:context()} |
    {stop, Reason::term(), state(), gen_cop:context()}.

-callback handle_call(gen_cop:request(), gen_cop:from(), state(), gen_cop:context()) ->
    {reply, gen_cop:reply(), state(), gen_cop:context()} |
    {noreply, state(), gen_cop:context()} |
    {stop, Reason::term(), state(), gen_cop:context()} |
    {stop, gen_cop:reply(), Reason::term(), state(), gen_cop:context()}.

-callback handle_cast(gen_cop:request(), state(), gen_cop:context()) ->
    {noreply, state(), gen_cop:context()} |
    {stop, Reason::term(), state(), gen_cop:context()}.

-callback handle_info(gen_cop:info(), state(), gen_cop:context()) ->
    {noreply, state(), gen_cop:context()} |
    {stop, Reason::term(), state(), gen_cop:context()}.

-callback terminate(Reason::term(), state(), gen_cop:context()) ->
    gen_cop:context().

-callback code_change(OldVsn::term(), state(), Extra::term()) ->
    {ok, state()} |
    {error, Reason::term()}.

%% gen_cop:delegate_info, delegate_data, delegate_call, delegate_cast
%% gen_cop:swap().
%% gen_cop:remove().
%% gen_cop:add().
