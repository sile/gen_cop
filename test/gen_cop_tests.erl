%% coding: latin-1
%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
-module(gen_cop_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(assertDown(Pid, ExpectedReason),
        (fun () ->
                 __Monitor = monitor(process, Pid),
                 receive
                     {'DOWN', __Monitor, _, _, __Reason} -> ?assertMatch(ExpectedReason, __Reason)
                 after 50 -> ?assert(timeout)
                 end
         end)()).

-define(assertDown(Pid, ExitReason, ExpectedReason),
        (fun () ->
                 __Monitor = monitor(process, Pid),
                 _ = exit(Pid, ExitReason),
                 receive
                     {'DOWN', __Monitor, _, _, __Reason} -> ?assertMatch(ExpectedReason, __Reason)
                 after 50 -> ?assert(timeout)
                 end
         end)()).

-define(assertAbort(Pid, ExpectedReason),
        (fun () ->
                 receive
                     {'EXIT', Pid, __Reason} -> ?assertMatch(ExpectedReason, __Reason)
                 after 50 -> ?assert(timeout)
                 end
         end)()).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
start_test_() ->
    [
     {"プロセス起動: 基本的な起動",
      fun () ->
              {ok, Socket} = gen_tcp:listen(0, []),
              Codec = make_dummy_codec(),

              Result = gen_cop:start(Socket, Codec, []),
              ?assertMatch({ok, _}, Result),
              {ok, Pid} = Result,

              ?assertDown(Pid, kill, killed)
      end},
     {"プロセス起動: リンク付きの起動",
      fun () ->
              {ok, Socket} = gen_tcp:listen(0, []),
              Codec = make_dummy_codec(),

              Result = gen_cop:start_link(Socket, Codec, []),
              ?assertMatch({ok, _}, Result),
              {ok, Pid} = Result,

              _ = process_flag(trap_exit, true),
              true = exit(Pid, kill),
              ?assertAbort(Pid, killed)
      end},
     {"プロセス起動: 名前付き",
      fun () ->
              Name = hoge_name,
              {ok, Socket} = gen_tcp:listen(0, []),
              Codec = make_dummy_codec(),

              Result = gen_cop:start(Socket, Codec, [], [{name, {local, Name}}]),
              ?assertMatch({ok, _}, Result),
              {ok, Pid} = Result,

              ?assertEqual(Pid, whereis(Name)),

              ?assertDown(Pid, kill, killed)
      end},
     {"プロセス起動: 名前の衝突",
      fun () ->
              Name = hoge_name,
              {ok, Socket} = gen_tcp:listen(0, []),
              Codec = make_dummy_codec(),

              {ok, Pid} = gen_cop:start(Socket, Codec, [], [{name, {local, Name}}]),

              %% 衝突
              ?assertEqual({error, {already_started, Pid}}, gen_cop:start(Socket, Codec, [], [{name, {local, Name}}])),

              ?assertDown(Pid, kill, killed)
      end},
     {"プロセス起動: asyncオプションが有効 (初期化完了まで待たない)",
      fun () ->
              Codec = make_dummy_codec(),
              UndefinedHandlerSpecs = [{undefined_module, []}], % 不正なハンドラ

              %% `{async, false}'の場合(デフォルト)は、start関数呼び出しに失敗する
              {ok, Socket0} = gen_tcp:listen(0, []),
              ?assertMatch({error, {'EXIT', {error, undef,  _}}},
                           gen_cop:start(Socket0, Codec, UndefinedHandlerSpecs)),

              %% `{async, true}'の場合は、起動は初期化処理を待たずに成功する
              {ok, Socket1} = gen_tcp:listen(0, []),
              Result = gen_cop:start(Socket1, Codec, UndefinedHandlerSpecs, [{async, true}]),
              ?assertMatch({ok, _}, Result),
              {ok, Pid} = Result,

              ?assertDown(Pid, {'EXIT', {error, undef, _}})
      end}
    ].

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec make_dummy_codec() -> gen_cop_codec:codec().
make_dummy_codec() ->
    gen_cop_codec:make([],
                       fun (_, Enc) -> {ok, [], Enc} end,
                       fun (_, Dec) -> {ok, [], Dec} end).
