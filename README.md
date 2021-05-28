gen_cop
=======

[![Build Status](https://travis-ci.org/sile/gen_cop.svg?branch=master)](https://travis-ci.org/sile/gen_cop)
[![Code Coverage](https://codecov.io/gh/sile/gen_cop/branch/master/graph/badge.svg)](https://codecov.io/gh/sile/gen_cop/branch/master)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## 概要

`gen_cop` は特定のクライアントを相手にTCPでコネクションを確立して通信するプロセスを作成するためのライブラリであり，Erlangの標準の `gen_tcp` モジュールが提供する，ソケットに対するcontrolling processの概念をより高級に扱えるようにラップしたもの．`cop` は “Connection-Oriented Protocol” の頭字語で，`gen_cop` を用いてトランスポート層のコネクションの確立を前提とした上位のプロトコルに基づいてサーバ側で動作するプロセスが実装できることを指す．

前提： ソケットの **controlling process** とは，TCPのソケット `Socket` と（`gen_tcp:controlling_process/2` によって）関連づけられ，`Socket` に外部から送られてきたバイト列を `{tcp, Socket, Dat}` の形のメッセージで受け取れるようになっているプロセスのこと．

* 参考： [controlling\_process/2 | Erlang -- gen\_tcp](https://erlang.org/doc/man/gen_tcp.html#controlling_process-2)

`gen_cop` によって生成されるプロセスを以降単に **`gen_cop` サーバプロセス** と呼ぶことにする．`gen_cop` サーバプロセスでは，ソケットを介してクライアント側（双方向通信だが便宜的に以後こう呼ぶ）から届いたデータを扱ったりソケットを介してクライアント側にデータを送ったりする **ハンドラ** という機構が列になって並んでおり，このうちの1つに「今有効なものはこれ」というフォーカスが当たっている．フォーカスの当たっているハンドラが実際に処理を担い，他のハンドラはフォーカスが当たるまで待機する．

また，`gen_cop` サーバプロセスは（クライアント側から届くデータだけでなく）別プロセスから `gen_server:cast` か `gen_server:call` で送られたメッセージおよびその他一般のメッセージを受け取ることができ，この受け取り処理もそのときにフォーカスの当たっているハンドラが行なう．

```
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ gen_cop Server Process                                        ┃
┃               ┏━━━━FOCUS━━━━┓                                 ┃
┃ ┌───────────┐ ┃┌───────────┐┃ ┌───────────┐     ┌───────────┐ ┃
┃ │ Handler 1 ├─┃┤ Handler 2 ├┃─┤ Handler 3 ├─ … ─┤ Handler N │ ┃
┃ └───────────┘ ┃└───────────┘┃ └───────────┘     └───────────┘ ┃
┃               ┗━┯━━━━━━━━┯━━┛                                 ┃
┃                 │ ┌────┐ │↑ data()                            ┃
┃                 │ │ ┌──┴─┴──┐                                 ┃
┃                 │ │ │ Codec │                                 ┃
┃                 │ │ └──┬─┬──┘                        binary() ┃
┃                 │ │    │ └────────────────────────────────────╂─ ← received via the associated socket
┃                 │ │    │                                      ┃
┃                 │ │    │                             iodata() ┃
┃                 │ │    └──────────────────────────────────────╂─ → sent via the associated socket
┗━━━━━━━━━━━━━━━━━┿━┿━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
        request() │ │ data()
                  │ │
                  │ └── ← received from other processes via gen_cop:send/2
                  └──── ← received from other processes via gen_cop:call/3 or gen_cop:cast/2
```

各ハンドラは，クライアント側からソケットを介して届いたバイト列や，別プロセスから送られたメッセージが自分の対処するものではないと判断して，ハンドラ列上で次にあるハンドラに処理を委任 (= delegate) することもできる．ハンドラから委任の旨が示されると，`gen_cop` サーバプロセスはフォーカスを次のハンドラに移し，以降はそのハンドラがソケットから届くバイト列や他プロセスから送られるメッセージの処理を担う．

`gen_cop` では，クライアント側からソケットに送られてきたバイト列をデコードして得られたデータを `data()`（または `message()`）という型で扱う（これは一般にコーデックごとに異なる型の実体をもつ）．これらのデータを以後 **受信データ** と呼ぶことにする．また，`gen_server:call` や `gen_server:cast` によって送られてくるプロセス間メッセージを **リクエスト** と呼ぶ．


## 主要API

### `gen_cop` モジュール

#### `from()`

```erlang
-type from() :: {pid(), Tag :: term()}
```

内部メッセージの送信元を表すデータの型．`gen_cop_handler` ビヘイビア参照．

* ※ `gen_cop` モジュールの実装ではこの型は `term()` と定義しているが，実装としては `gen_server` ビヘイビアのコールバック函数 `handle_call/3` の第2引数に与えられる形式と同じである．


#### `data()`（または `message()`）

```erlang
-type data() :: term().
```

`gen_cop:send/2` の第2引数に渡すとエンコードされて最終的にTCPでソケットを介してクライアント側に送信されるデータ，またはクライアントからの受信データの型であり，用いる `gen_cop_codec` ビヘイビアを実装したモジュールごとに（すなわち送受信されるデータ用のコーデックごとに）異なる．`gen_cop_codec:codec()` 参照．


#### `request()`

```erlang
-type request() :: term().
```

`gen_cop` サーバプロセスが同期的または非同期的に受けつけるリクエストの型であり，一般にはハンドラごとに異なる．`gen_cop_handler` ビヘイビア参照．

ただし，以下のいずれかの形式にマッチする値は内部表現用に予約されており，`request()` 型の値としては使えない：

* `get_logger`
* `which_handlers`
* `{'$send', Data}`


#### `start/4`，`start_opts()`

```erlang
-spec start(
    inet:socket(),
    gen_cop_codec:codec(),
    [gen_cop_handler:spec()],
    start_opts()
) ->
    {ok, pid()}
  | {error, start_err()}.

-type start_err() :: {already_started, pid()} | timeout | term().
```

`start(Socket, Codec, HandlerSpecs, Options)` で関連づけるソケットを `Socket`，用いるコーデックを `Codec`，ハンドラ列の指定を `HandlerSpecs` として `gen_cop` サーバプロセスを立ち上げる．起動オプションは以下：

```erlang
-type start_opts() :: [start_opt()].

-type start_opt() ::
    {link,          boolean()}
  | {name,          otp_name()}
  | {ack_fun,       ack_fun()}
  | {async,         boolean()}
  | {owner,         pid()}
  | {on_owner_down, on_owner_down()}
  | {spawn_opt,     [term()]} % TODO: more specific typespec
  | {timeout,       timeout()}
  | {debug,         [term()]} % TODO: more specific typespec
  | {logger,        logi:context()}.

-type ack_fun() :: fun(() -> any()).

-type on_owner_down() :: fun((OwerPid :: pid(), OwnerExitReason :: term()) -> ExitReason :: term()).
```

* `link`
  - 呼び出したプロセスとの間にリンクを張るか否か．
  - デフォルトは `false` で，リンクを張らない．
* `name`
  - 名前つきで起動する際の指定．
  - 指定しなかった場合は名前なし．
* `ack_fun`
  - `gen_cop` サーバプロセスの初期化時にそのプロセスで呼び出される，サーバプロセスをソケットのcontrolling processにするための処理の指定．
  - 省略された場合，`gen_cop:start/4` を呼び出した側のプロセスで `gen_tcp:controlling_process(Socket, ServerPid)` が評価され（ただし `ServerPid` は立ち上がったばかりの `gen_cop` サーバプロセスのPID），起動処理と並行してソケットとプロセスの関連づけを行なう処理が走る．
* `async`
  - 起動処理を非同期にするか否か．
  - `true` を指定すると，`gen_cop` プロセスが起動した際の文脈の初期化処理が完了するのを待たずに戻り値が返る．
    * 文脈については後述の `gen_cop_context:context()` を参照．
  - デフォルトでは `false`，すなわち初期化完了を待つ同期的な起動を行なう．
* `timeout`
  - `async` が `false` の場合に，初期化完了を最大でどれだけ待つか．
  - デフォルトは `infinity`．
* `owner`
  - 生成した `gen_cop` プロセス **から** モニタを張って監視する対象のプロセス．
  - この監視対象のプロセスが死んだ場合は `gen_cop` プロセスもそれを受けて終了する．
  - 省略した場合は特にどのプロセスも監視しない．
* `on_owner_down`
  - `owner` に指定したプロセスが死んでDOWNのメッセージが届いた際に起動される終了処理．
  - 省略した場合は `gen_cop:default_on_owner_down/2` が用いられる．
* `spawn_opt`
  - 内部的にプロセスの起動に使用する `proc_lib:spawn_opt/4` に与えるオプション．
  - `{link, true}` を指定している場合は，ここに `link` は指定しなくてよい．
  - 省略した場合は（`link` の有無のみ別として）`[]`．
* `debug`
  - `gen_cop` サーバプロセスをOTP準拠のプロセスとするために要請される **debug structure** を作成する過程で `sys:debug_options/1` に与えるオプション．
    * 参考： [Erlang -- sys and proc\_lib](https://erlang.org/doc/design_principles/spec_proc.html)
  - 省略時は `[]` が与えられた場合と等価．
* `logger`
  - `logi` モジュールのlogger instanceの指定．
  - 省略された場合は `logi:make_context/0` により新たにつくられる．


#### `otp_ref()`

```erlang
-type otp_ref() ::
    (Name :: atom())
  | {Name :: atom(), node()}
  | {global, Name :: term()}
  | {via, module(), Name :: term()}
  | pid().
```

後述の `send/2`，`call/3`，`cast/2` で使われる，`gen_cop` サーバプロセスを指し示すデータの型．OTPで用いられる形式と同じ．


#### `send/2`

```erlang
-spec send(otp_ref(), data()) -> ok.
```

第1引数に与えた `gen_cop` サーバプロセスに紐づいているソケットを介してクライアント側に第2引数のデータを送る．データは `gen_cop` サーバプロセスのもつコーデックによりエンコードされる．非同期処理であり，戻り値 `ok` は `gen_cop` サーバプロセスへの指示が完了した時点で返る．


#### `call/3`

```erlang
-spec call(otp_ref(), request(), timeout()) -> term().
```

`gen_cop` サーバプロセスに対して同期的なリクエストを送る．フォーカスの当たっているハンドラからの返信が戻り値となる．


#### `cast/2`

```erlang
-spec call(otp_ref(), request()) -> ok.
```

`gen_cop` サーバプロセスに対して非同期的なリクエストを送る．戻り値は送信が終わると即座に返る．


### `gen_cop_codec` モジュール

送受信されたデータのエンコード・デコードを担う．

#### `codec()`，`codec_state()`

`codec()` はコーデックの型．基本的にはエンコーダとデコーダの組だが，バイト列が受信データの途中で切れていてデコード時に未処理部分として余る状況や，デコード処理そのものが文脈依存の場合にも対応できるように，エンコード・デコード用の状態も保持する．この状態の型は `codec_state() :: term()` で表される．

* ※ `codec()` 型の宣言は `-opaque` ではないが，実質的に抽象型．

* ※ 型の気持ちとしてはデコード中の状態の型 `T` と最終的にデコードして得られるメッセージの型 `M` をパラメータとして `codec(T, M)` という要領だが，ここでの `T` に相当するものは `gen_cop_codec:codec_state() :: term()`，`M` に相当するものは `gen_cop:message() :: term()` とそれぞれ表され，パラメータが暗黙化された `codec()` という定式化にしている．


#### `codec_module()`

`gen_cop_codec` ビヘイビアを実装したモジュールの名前の型．

```erlang
-type codec_module() :: module().
```


### `encode_result()`，`decode_result()`

エンコード結果とデコード結果の型．`gen_cop:message()` と `codec_state()` 型に依存して以下のように定められる．エンコード結果の本体は `binary()` ではなく `iodata()` であることに注意：

```erlang
-type encode_result() :: encode_result(codec_state()).

-type encode_result(State) ::
    {ok, iodata(), State, gen_cop_context:context()}
  | {error, Reason :: term(), State, gen_cop_context:context()}.

-type decode_result() :: decode_result(gen_cop:message(), codec_state()).

-type decode_result(Message, State) ::
    {ok, [Message], State, gen_cop_context:context()}
  | {error, Reason :: term(), State, gen_cop_context:context()}.
```


#### `make/2`

```erlang
-spec make(codec_module(), codec_state()) -> codec().
```

エンコーダとデコーダの組を `gen_cop_codec` ビヘイビアを実装したモジュールとして第1引数に，デコード・エンコード用の状態として初期状態を第2引数にそれぞれ与えてコーデックを作る．


#### `make/3`，`encode_fun()`，`decode_fun()`

```erlang
-spec make(codec_state(), encode_fun(), decode_fun()) -> codec().

-type encode_fun() ::
    fun((
        [gen_cop:message()],
        codec_state(),
        gen_cop_context:context()
    ) ->
        encode_result()).

-type decode_fun() ::
    fun((
        binary(),
        codec_state(),
        gen_cop_context:context()
    ) ->
        decode_result()).
```

`make/2` と同用途だが，モジュールを介さず，直接エンコード用函数とデコード用函数を直接渡してコーデックを作る．`encode_fun()` と `decode_fun()` は `gen_cop_codec` ビヘイビアが要請する `encode/3` と `decode/3` の型とそれぞれ同じ．


#### `get_state/1`，`set_state/2`

```erlang
-spec get_state(codec()) -> codec_state().
-spec set_state(codec_state(), codec()) -> codec().
```

デコード・エンコード中の状態をそれぞれ取得・設定する．


#### `encode/3`

```erlang
-spec encode([gen_cop:message()], codec(), gen_cop_context:context()) -> encode_result().
```

`encode(Messages, Codec, Context)` でコーデック `Codec` を用いてメッセージ列 `Messages` をエンコードしつつ文脈 `Context` を更新する．


#### `decode/3`

```erlang
-spec decode(binary(), codec(), gen_cop_context:context()) -> decode_result().
```

`decode(Bin, Codec, Context)` でコーデック `Codec` を用いてバイナリ `Bin` をメッセージ列へデコードしつつ `Context` を更新する．


### `gen_cop_codec` ビヘイビア

エンコード・デコードの実装を与えるためのビヘイビア．コールバックとして以下を要請する：

```erlang
-callback encode([gen_cop:message()], codec_state(), gen_cop_context:context()) -> encode_result().
-callback decode(binary(), codec_state(), gen_cop_context:context()) -> decode_result().
```

`codec_state()` はコーデックごとに異なるような状態の形式であり，例えばパケットとして送られてくるバイト列がデータ列にデコードできない中途半端なところで切れる場合に未処理のバイト列を保持するなどの用途に使える．

* ※ `gen_cop:message() :: term()` はデコードされたデータの型であり，コールバックモジュールごとに異なるので実質的にパラメータである．


### `gen_cop_handler` モジュール

#### `spec()`

```erlang
-type spec() ::
    {module(), arg()}
  | {module(), arg(), spec_opts()}.

-type spec_opts() :: [spec_opt()].

-type spec_opt() ::
    {id,       id()}
  | {callback, callback_opts()}.

-type callback_opts() :: [callback_opt()].

-type callback_opt() ::
    {init,        init_fun()}
  | {handle_data, handle_data_fun()}
  | {handle_call, handle_call_fun()}
  | {handle_cast, handle_cast_fun()}
  | {handle_info, handle_info_fun()}
  | {terminate,   terminate_fun()}
  | {code_change, code_change_fun()}.
```

`spec()` は，ハンドラの振舞いの指定に相当するデータである， `gen_cop_handler` ビヘイビアを実装したモジュールの名前 `Module` とその `Module:init/2` の第1引数に与える `Arg :: arg()` の組 `{Module, Arg}` につく型（`arg() :: term()` については `gen_cop_handler` ビヘイビアの節を参照）．オプションを伴って `{Module, Arg, Opts}` とすることもできる．オプションのないものは `[]` がオプションに指定されたのと同じ扱い．

オプション：

* `id`
  - ハンドラのIDの指定．
  - 省略した場合はモジュール名 `Module` が使われる．
* `callback`
  - `Module` が提供するコールバック函数（の一部）を適宜上書きして利用したい場合に指定する．


### `gen_cop_handler` ビヘイビア

ハンドラの実装に要請されるビヘイビア．以下のコールバック函数を要請する：

```erlang
-type state() :: term().
-callback init(arg(), context())                                             -> handle_result().
-callback handle_data(gen_cop:data(), state(), context())                    -> handle_result().
-callback handle_call(gen_cop:request(), gen_cop:from(), state(), context()) -> handle_result().
-callback handle_cast(gen_cop:request(), state(), context())                 -> handle_result().
-callback handle_info(gen_cop:info(), state(), context())                    -> handle_result().
-callback terminate(Reason :: term(), state(), context())                    -> handle_result().
-callback code_change(OldVsn :: term(), state(), Extra :: term(), context()) -> handle_result().
```

ただし， `context()` と `handle_result()` は以下のようなエイリアス：

```erlang
-type context()       :: gen_cop_context:context().
-type handle_result() :: gen_cop_context:handler_result().
```

* ※ `state()` と `arg()` は一般にコールバックモジュールごとに異なるので実質パラメータ．


### `gen_cop_context` モジュール

#### `context()`

```erlang
-opaque context().
```

通信の文脈を保持するデータの抽象型．ソケットやコーデック情報のほかハンドラの列を保持しており，またその列の要素のうちのどれにフォーカスが当たっていてアクティブであるかの情報ももっている．届いたデータやリクエストをさばく処理はそのフォーカスが当たっているハンドラのコールバック函数が担う．


#### `handler_result()`

主に `gen_cop_handler` ビヘイビアで使用される，文脈の更新処理の戻り値．

```erlang
-type handler_result() :: handler_result(term()).

-type handler_result(Reason) :: {ok, context()} | {stop, Reason, context()}.
```


#### `ok/2`

```erlang
-spec ok(gen_cop_handler:state(), context()) -> handler_sesult().
```

主に `gen_cop_handler` ビヘイビアの各コールバック函数中で戻り値をつくるのに使われる．`ok(State, Context)` でフォーカスの当たっているハンドラの状態を `State` にするように文脈 `Context` を更新したものを返す．


#### `ok/1`

```erlang
-spec ok(context()) -> handler_result().
```

文脈をリセットする（つまりフォーカスの当たるハンドラを先頭に戻す）．

* ※ APIから文脈をリセットする方法は，正常系ではこの函数のみ．


#### `delegate_data/{2,3}`

```erlang
-spec delegate_data(gen_cop:data(), gen_cop_handler:state(), context()) -> handler_result().
```

典型的には `gen_cop_handler` ビヘイビアを実装したモジュールのコールバック函数 `handle_data/3` の中で戻り値を作るのに使う．「受信データに対し，このハンドラでは何もせず，ハンドラ列上の次のハンドラにフォーカスを移して委任する」ことを表す値を返す．ハンドラの状態を更新しない場合は 第2引数を省略して `delegate_data/2` を用いてもよい．


#### `delegate_call/{3,4}`

```erlang
-spec delegate_call(term(), gen_cop:from(), state(), context()) -> handler_result().
```

典型的には `gen_cop_handler` ビヘイビアを実装したモジュールのコールバック函数 `handle_call/4` の中で戻り値を作るのに使う．「`gen_server:call/{2,3}` で送られてきた同期的リクエストに対し，このハンドラでは何もせず，ハンドラ列上の次のハンドラにフォーカスを移して委任する」ことを表す値を返す．ハンドラの状態を更新しない場合は 第3引数を省略して `delegate_call/3` を用いてもよい．


#### `delegate_cast/3`

```erlang
-spec delegate_cast(term(), gen_cop_handler:state(), context()) -> handler_result().
```

`delegate_call/4` の非同期リクエスト版．


#### `remove_handler/3`

```erlang
-spec remove_handler(
    gen_cop_handler:id(),
    RemoveReason :: term(),
    context()
) ->
    {ok, context()}
  | {error, ErrorReason, context()}
when
    ErrorReason :: not_found | in_active | term().
```

`remove_handler(Id, RemoveReason, Context)` でIDが `Id` であるようなハンドラを文脈 `Context` から除去する．指定したIDに対応するハンドラが見つからなかったときは `not_found` のエラーが，IDに対応するハンドラが現在フォーカスの当たっているハンドラだった場合は `in_active` のエラーがそれぞれ返される．

この処理の際に，除去されるハンドラに対してコールバック函数の `HandlerMod:terminate(RemoveReason, HandlerState, ContextIn)` が呼び出される（ただし，`ContextIn` はハンドラ列を `HandlerMod` のみとするような文脈へと `Context` を更新したもの）．このコールバック函数の呼び出しが `{ok, ContextOut}` を返したら，`ContextOut` のハンドラ列を（正常に `Id` が除去されてフォーカスがそのままの状態へと）元に戻して `Context1` とし，戻り値を `{ok, Context1}` とする．


#### `get_codec/1`

```erlang
-spec get_codec(context()) -> gen_cop_codec:codec().
```

文脈からコーデックを取り出す．


#### `set_codec/2`

```erlang
-spec set_codec(gen_cop_codec:codec(), codec(), context()) -> context().
```

文脈中のコーデックを更新する．


## 内部実装

以下はAPIではなく内部実装についての記載．


### `gen_cop_handler` モジュール

ハンドラ1つごとの処理を扱っている．処理自体はおおよそ `gen_cop_handler` ビヘイビアを実装したモジュールへ丸投げされている．


#### `header()`

```erlang
-define(HEADER, ?MODULE).

-record(?HEADER, {
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

-opaque header() :: #?HEADER{}.
```

* `module`
  - `gen_cop_handler` ビヘイビアを実装したモジュールの名前が保持される．
  - `make_instance/1` によって引数の `spec()` 型の値から取り出されてここに設定される．
* `id`
  - `spec()` のオプションのIDが取り出されてここに設定される．オプションで指定のない場合はモジュール名が転用される．
* 他：
  - 基本的にはフィールド名に対応する `Module` のコールバック函数を保持するが，`spec()` のオプションの `callback` に対応する函数名の指定があった場合はそちらを優先して保持する．


#### `handler()`，`uninitialized_handler()`，`initialized_handler()`

```erlang
-type handler() :: uninitialized_handler() | initialized_handler().
-type uninitialized_handler() :: {header(), arg()}.
-type initialized_handler() :: {header(), state()}.
-type state() :: term().
```

`uninitialized_handler()` は `{Module, Arg, Opts} :: spec()` の `Module` と `Opts` から `make_instance/1` で `Header :: header()` をつくって `{Header, Arg}` としたものの型．引数が `Module:init/2` に渡される初期化が済んでいないという意味でuninitialized．


#### `make_instance/1`

```erlang
-spec make_instance(spec()) -> uninitialized_handler().
```

`spec()` での指定をもとに未初期化のハンドラを作る．


#### `init/2`

```erlang
-spec init(uninitialized_handler(), context()) -> gen_cop_context:handler_result().
```

`init({Header, Arg}, Context)` でほぼ単に `Header:init(Arg, Context)` を呼び出すだけ．


#### `handle_data/2`

```erlang
-spec handle_data(
    gen_cop:data(),
    intitialized_handler(),
    gen_cop_context:context()
) ->
    gen_cop_context:handler_result().
```

`handle_data(Data, {Header, State}, Context)` でほぼ単に `Header:handle_data(Data, State, Context)` を呼び出すだけ．


### `gen_cop_context` モジュール

#### `context()`

通信用のソケットやコーデックなどをまとめて文脈としたもの．

```erlang
-record(?CONTEXT, {
    %% TODO: user_state (connection global state)
    socket             :: inet:socket(),
    codec              :: gen_cop_codec:codec(),
    handlers      = [] :: [gen_cop_handler:handler()], % XXX: non-empty check
    done_handlers = [] :: [gen_cop_handler:handler()],
    send_queue    = [] :: [term()] % TODO: type
}).

-opaque context() :: #?CONTEXT{}.
```

`socket` は初期化以降は変更されない．`codec` は初期化以降はAPIの `set_codec/2` で明示的に更新される以外での変更はない．初期化が終わった段階では `handlers` に `[initialized_handler()]` 型のハンドラ列が入っており，`done_handlers` は空リスト．

`handlers` と `done_handlers` がメッセージハンドリングの中核をなす．`handlers` のリストの先頭にあるハンドラが “フォーカスの当たっている” ハンドラで，これが受信データやリクエストを処理する．

フォーカスは `delegate_{data,call,cast,info}` が呼ばれることによって後続のハンドラへと移り，フォーカスを外れたハンドラは `done_handlers` にアキュムレータとして蓄積される．


#### `init/3`

文脈の初期化．同一IDのハンドラが複数指定されている場合はエラーが返る．

```erlang
-spec init(
    inet:socket(),
    gen_cop_codec:codec(),
    [gen_cop_handler:uninitialized_handler()]
) ->
    {ok, context()}
  | {stop, Reason}
when
    Reason :: {already_present, gen_cop_handler:id()} | term().
```

第3引数に指定されたリストの後ろの要素から順に `handlers` フィールドに追加しつつ（したがって最終的に `handlers` フィールドのリストは正順になる），`gen_cop_handler:init/2` を呼び出して初期化している（各ハンドラの初期化で（正常なら）`gen_cop_context:ok/2` が呼ばれ，その際にハンドラ列中の対応するハンドラの状態が登録されて `uninitialized_handler()` 型の値が `intitialized_handler()` 型の値に置き換えられる）．以降文脈中の `handlers` のハンドラ列はすべて `initialized_handler()` の形となる．


#### `next_handler/2`

```erlang
-spec next_handler(gen_cop_handler:state(), context()) -> context().
```

文脈中で畳み込み用に現在フォーカスされているハンドラ（＝ `handlers` フィールドのリストの先頭にあるハンドラ）を `done_handlers` に移し，フォーカスを次のハンドラに当てる．


#### `fix_handlers/1`

```erlang
-spec fix_handlers(context()) -> context().
```

`done_handlers` に移していたハンドラたちを元の順番で `handlers` に戻し，畳み込み中でない形へリセットする．


#### `flush_send_queue/1`

```erlang
-spec flush_send_queue(context()) -> {ok, iodata(), context()} | {error, Reason :: term(), context()}.
```

文脈の `send_queue` フィールドに溜まったメッセージ列を `iodata()` にエンコードし，かつ文脈の `send_queue` を空にしてそれぞれを返す．


#### `recv/2`

```erlang
-spec recv(binary(), context()) -> handler_result().
```

`recv(Bin, Context)` でソケットから受信したバイナリ `Bin` を処理する．文脈 `Context` 中に保持しているコーデック `Codec` を用いて `gen_cop_codec:decode(Bin, Codec, Context)` を呼び出してメッセージ列へとデコードし，そのメッセージ列を `handle_data` で舐めて文脈を畳み込む．


### `gen_cop_server` モジュール

プロセスの起動やメインループを担う．“`gen_server` 形式” のメッセージを受け取ることなどを想定した実装になっている．


#### `flush_send_queue/1`

内部函数．文脈の `send_queue` に溜まったメッセージを `gen_cop_context:flush_send_queue/1` で `iodata()` にエンコードして取り出し，`gen_tcp:send/2` でソケットを介して送信する．


#### `handle_recv/2`

内部函数．ソケットからデータ `{tcp, …, Bin :: binary()}` を受信すると `handle_recv(Bin, State)` で呼び出される（ただし `State` はメインループで保持されている `gen_cop` プロセスの状態）．これは `gen_cop_context:recv/2` を呼び出すことで `State` の `context` フィールドに保持されている文脈を更新する．
