## 重要コマンド

### フォーマット

基本的にファイルはツールで自動フォーマットしています。

#### nix fmt

[treefmt-nix](https://github.com/numtide/treefmt-nix)が対応しているファイルは以下のコマンドでフォーマット出来ます。

```console
nix fmt
```

Haskellのモジュールを追加した時や削除した時は、
[cabal-gild](https://hackage.haskell.org/package/cabal-gild)によるフォーマットで変更を反映する必要があります。
cabal-gildは統合されているので`nix fmt`で実行出来ます。

#### cargo

Rustのフォーマットはrustfmtだけはtreefmtが対応していますが、
他のlinterには対応していないので以下のコマンドでチェックと自動修正を利用してください。

```console
cargo fix --allow-dirty
cargo clippy --fix --allow-dirty
```

### 統合チェック

以下のnixコマンドで、プロジェクト全体のフォーマットチェック・ビルド・テストが行えます。

```console
nix flake check
```

### ビルド

#### Haskell

```console
cabal build --disable-optimization --enable-tests all
```

#### Rust

```console
cargo build
```

### テスト

#### Haskell

```console
cabal test --disable-optimization --enable-tests all
```

#### Rust

```console
cargo test
```

## 使用する技術スタックやライブラリ

環境構築には[Nix Flakes](https://wiki.nixos.org/wiki/Flakes/ja)を利用しています。
Nix FlakesでHaskell部分を管理するには[haskell.nix](https://input-output-hk.github.io/haskell.nix/)を使っています。

## Haskell

### 言語設定

Haskellの言語バージョンや言語拡張はプロジェクトレベルで以下を設定しています。

```cabal
default-language: GHC2021
default-extensions:
  DataKinds
  DefaultSignatures
  DerivingStrategies
  DerivingVia
  DuplicateRecordFields
  FunctionalDependencies
  GADTs
  LambdaCase
  MultiWayIf
  NoFieldSelectors
  NoImplicitPrelude
  NoImportQualifiedPost
  OverloadedStrings
  PartialTypeSignatures
  QuasiQuotes
  RecordWildCards
  StrictData
  TemplateHaskell
  TypeFamilies
  ViewPatterns
```

特に`NoFieldSelectors`が有効になっていることに注意してください。
これによりフィールドアクセスに単純な関数を使うことは出来なくなりますが、
代わりにレコードの同名フィールドが簡単に扱えるようになってプレフィクスをつける必要がなくなります。
フィールドへのアクセスはlensを使ってください。

### named importやqualified importを控える

必要が無いときはnamed importやqualified importは控えてください。
今はhaskell-language-serverがあるので、
どれが何をimportしているのかは簡単に把握できるので、
コードが複雑に見えて美しくありません。

もちろん命名がコンフリクトした場合は適切に適度に使用してください。

### exportは明示的に列挙する

全てのシンボルをexportで公開するのは禁止です。

外部に公開しないといけないシンボルだけをexportしてください。

明示的に列挙することで公開しているAPIが明確になり、
デッドコードの検出なども簡単になります。

昔は型定義にlensで`makeFieldsId`を使った場合などはあまりにもシンボルが膨大すぎるので止むを得ず許可していましたが、
もう人間はGitHub Copilot Chatの補完が使えるのでそんなに列挙するのは苦ではありませんし、
LLMのエージェントは人間ではないので自分で列挙できます。

### re-export禁止

importしたシンボルやモジュールを再度exportするのは禁止。

まず並列コンパイルのことを考えると依存関係は最小限に留めたいという理由があります。

またfourmoluの演算子の優先順位パースがre exportだとしばしば壊れるという理由もあります。。

より深刻な理由としては相互参照の発生です。
Haskellではモジュールの相互参照は実行時にリンクを行うJVM言語ほど楽には出来ないので、
たくさんのモジュールをexportやimportして依存関係を作ってしまうと、
循環参照が発生した時に解決するのが困難になっていきます。
なので面倒でも一つ一つimportしていくべきです。

唯一の例外はライブラリとしてまとまったAPIを公開する時ですが、
それは注意深く決定するので自動的に作成するべきではありません。

### lensで定義されたアクセサは、アクセサ単体ではなく型クラスごとexportする

例えば`makeFieldsId`で`HasUser`型クラスと`user`アクセサを定義した場合、
`user`アクセサをexportするのではなく、
`HasUser`型クラスをexportしてください。

### モジュールは出来るだけ細かく分ける

Haskellではモジュールの相互参照が非常に面倒くさいという理由から、
モジュールをまとめることは簡単ですが、
モジュールを分割することは難しいです。

そのためHaskellにおいてはコンパイラが警告しない限りはモジュールを分割して困るということはあまりありません。

また並列コンパイルや差分コンパイルの効率もモジュールは分割したほうが良くなります。

よってモジュールは出来るだけ細かく分割してください。

特にTemplate Haskellのコードはモジュール分割や定義順序が重要なので、
型に対して`makeFieldsId`を使う場合などは、
型の定義と`makeFieldsId`でアクセサを定義することを行うだけのモジュールを作成することを推奨します。

### 部分関数の禁止

純粋関数なのに例外を頻繁に投げる以下のような関数の使用は禁止です。
RIOを使うことで基本的に避けられますが、importもしないでください。

- `fromJust`
- `read`

例えば`read`には`readMaybe`などの安全な代替関数があるので、そちらを使ってください。

### `error`関数の禁止

純粋関数空間の中で例外を投げられる`error`関数の使用は禁止です。
よほどの理由がない限りは正当化されません。
上位空間に`MonadThrow`や`Either`などを使って例外を伝播させてください。

### 例外は型をつけよう

`throwString`のような関数を使うより、
ちゃんと例外に型をつけて`throwM`などで型がついた例外を使いましょう。

例外を表現するデータ型はなるべく`Text`などの文字列を使うのではなく、
エラーが起きた理由を表現する構造的なデータ型をフィールドとして持ってください。

さらに、例外の型は、型自体はexportしても、型コンストラクタはexportしないでください。
代わりにその例外を内部で`throwM`で投げる関数をexportして、例外処理をわかりやすくしてください。

### エラーを握り潰すのは禁止

`IO`の文脈などで例外が生じた場合に握りつぶして何もしないような行為は禁止。
`IO`は文脈的に既に例外が発生する可能性があることを示しているので、
例外が上位に伝播することは許容されます。
特に動作上問題がない場合は警告などのレベルのログを出しておく。
問題が発生している場合は例外を上位に再伝達する。
そこで例外を処理するのが完全に適切なら警告を出して処理する。

例外だけではなく`Either`の`Left`なども適切に処理してください。
`Left`が来るのが正常系である場合はデフォルト値やフォールバック値を使ってください。
単に握りつぶすのは禁止です。

### `IO`的な文脈で`Either`や`Maybe`を包むのは推奨されない

`IO`は例外が発生する可能性がある文脈を十分に表現しているモナドなので、
その中で`Either`や`Maybe`を使って例外的な状況を示すのは二重にネストしていて混乱を招きます。
素直に例外を投げてしまうのが良いでしょう。

`IO`的な操作をしているが`IO`そのものではないモナドの場合は、
`MonadThrow`や`MonadIO`の型クラスが役に立つ場合があります。

ただし例外があり、
データベースを`lookup`するような操作は、
存在しないというデータが正常系として扱われるので、
その場合は`IO (Maybe a)`のようなシグネチャを使うことは適切です。

### `IO`をなるべく直接使わず型クラスを使う

`IO`モナドはあまりにもプリミティブなので他のモナド変換子などと一緒に取り扱うのが不便です。
出来る限り`MonadIO`, `MonadUnliftIO`と言った型クラスで抽象化するべきです。

#### `RIO`モナドを更に抽象化する

実際にどういう型に実行時に具体的になるかと言うと、基本的には以下の`RIO`型になります。

```haskell
newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
```

しかし`RIO`も直接使うのは推奨されません。
以下のようにより抽象的で汎用的な型クラスを使っておくと`RIO`を使っていないプロジェクトからも再利用が容易になります。

```haskell
foo :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, MonadThrow m) => m ()
```

具体的な型を使わないと最適化が効きにくい時もありますが、
早すぎる最適化は悪なので、
パフォーマンスチューニングをする時だけ具体的な型を使って調整します。

#### `RIO`の実行

`RIO`を実行して`IO`空間に変換する方法を説明します。

ロガーぐらいしか持たないシンプルな`env`しか使わない場合は、
[RIO.Prelude.Simple](https://hackage.haskell.org/package/rio/docs/RIO-Prelude-Simple.html)
に定義されている`runSimpleApp`関数で実行するだけで十分です。

```haskell
runSimpleApp :: MonadIO m => RIO SimpleApp a -> m a
```

複雑な文脈を持つモジュールの例では、
これに対して以下のように`ReaderT`が要求する`env`を作成して実行する。

```haskell
runEnv :: (MonadUnliftIO m, MonadThrow m) => RIO Env a -> m a
runEnv = do
  let isVerbose = False
  logOptions <- logOptionsHandle stderr isVerbose
  withLogFunc logOptions $ \lf -> do
    let env = -- ここで実際にアプリケーションに固有の文脈を作成する。
    runRIO env action
```

#### `IO`内部で`MonadUnliftIO`のアクションを実行する

`MonadIO`や`MonadUnliftIO`の文脈で`IO`のアクションを実行する場合は`liftIO`を使うだけで良い。

逆に`IO`の文脈で`MonadUnliftIO`のアクションを実行する場合はひと工夫必要。

以下の関数を使うことで解決できる。

```haskell
askRunInIO :: MonadUnliftIO m => m (m a -> IO a)
```

`askRunInIO`を`MonadUnliftIO`の文脈呼び出すことで、
`MonadUnliftIO`のアクションを`IO`に変換する関数を取得できる。

これを使うと`IO`を要求するライブラリの型に対して`MonadUnliftIO`のアクションを少しラムダ式で包んだりすれば渡すことが出来る。

### `String`の使用をなるべく避ける

`String`は`[Char]`のエイリアスであり、
Haskellの文字列を表現するために使われますが、
非常に非効率的であり、
さらに表示したときの日本語文字列がしばしばエスケープされます。
昔からある型のため仕方なく使われていますが、
なるべく`Text`を使ってください。

`Text`はUTF-8でエンコードされた文字列を効率的に扱うための型です。
基本的に`Text.Lazy`ではなくStrictな`Text`を使ってください。
`ByteString`との使い分けに関しては、
Unicodeで正しく表現できる文字列である場合`Text`を使い、
バイナリデータやエンコードが不明な場合は`ByteString`を使うのが正しいです。

しかし現代においてはUnicodeで表現できない文字列はあまりないので、
バッファ上のバイナリデータを使う場合などを除いては、
基本的に`Text`を使うことが適切でしょう。

`String`は古くから使われているからライブラリに蔓延しているだけのガンです。
使わなければいけない場合に一瞬`convert`などで変換するだけにしてください。

詳しく知りたい場合は、
[Haskellの文字列型：分類と特徴 #Haskell - Qiita](https://qiita.com/mod_poppo/items/740659702f31216fdade)
参照してください。

### mutableな変数の使用を避ける

Haskellに限らずmutableな変数は避けるべきだとされています。
特にHaskellは純粋関数型言語でありimmutableにレコード全体を差分更新することが前提とされて効率よく行えるようになっているので、
mutableな変数がほしいことはめったにありません。

スレッド間の通信などでトランザクションを作りたいとかのどうしても必要な理由でない場合、
mutableな変数を表す型は使わないでください。

### 代替Preludeの[rio: A standard library for Haskell](https://hackage.haskell.org/package/rio)

他のモジュールより、出来るだけRIOが提供する以下のモジュールを優先してimportしてください。

- RIO
- RIO.ByteString
- RIO.ByteString.Lazy
- RIO.Char
- RIO.Deque
- RIO.Directory
- RIO.File
- RIO.FilePath
- RIO.HashMap
- RIO.HashSet
- RIO.Lens
- RIO.List
- RIO.Map
- RIO.NonEmpty
- RIO.Process
- RIO.Seq
- RIO.Set
- RIO.State
- RIO.Text
- RIO.Text.Lazy
- RIO.Time
- RIO.Vector
- RIO.Vector.Boxed
- RIO.Vector.Storable
- RIO.Vector.Unboxed
- RIO.Writer

特に以下のモジュールのimportは禁止です。

- Data.ByteString
- Data.ByteString.Lazy
- Data.Text
- Data.Text.Lazy

RIOが直接exportしている範囲では足りない場合は、
RIOが依存していて既にプロジェクトで間接的に依存している以下のパッケージを優先して使ってください。

- mtl
- unliftio

### [convertible: Typeclasses and instances for converting between types](https://hackage.haskell.org/package/convertible)

`convert`関数で汎用的な型変換を行っています。
`pack`, `unpack`, `encodeUtf8`, `decodeUtf8`のような個別の関数よりなるべく`convert`を使うようにしてください。

convertibleをimportするときは単に以下のように書いてください。

```haskell
import Data.Convertible
```

### [lens: Lenses, Folds and Traversals](https://hackage.haskell.org/package/lens)

`makeFieldsId`というTemplate Haskell関数を使ってレコードのフィールドアクセサを定義してください。

`makeFieldsId`を使うときはフィールドにプレフィクスやアンダースコアは付けないでください。
`NoFieldSelectors`拡張の力でプレフィクスは不要になっています。
`makeFieldsId`関数は完全にフィールド名と同じアクセサを生成するので、
プレフィクスやアンダースコアをつけると奇妙なアクセサが生成されてしまうのでむしろよくありません。
フィールド名にアンダースコアを使うのは禁止。

`makeFieldsId`は実行する段階で既に型クラスの定義が見えれば、
型クラスの重複定義はせず、
既に存在する型クラスのインスタンスとしてアクセサを定義します。

そのため型クラスの重複を怖がってimportを少なくする必要はありません。
むしろ積極的に既存の型クラスをimportしてください。
循環参照などが発生した場合は型クラスの定義だけを別のモジュールに分割して、
双方それをimportするのも手です。

`makeFieldsId`関数を使うときは以下のようにimportを行ってください。

```haskell
import Control.Lens (makeFieldsId)
```

`Control.Lens`は`RIO`のexportしているシンボルと衝突しやすいから特別にnamed importを推奨しています。

その他のシンボルが必要になってからシンボルのimportを追加してください。
RIOと衝突したときは原則としてRIO側のシンボルを優先してください。
不要なときはimportしないでください。

### rioとlensの衝突しやすさ

rioとlensのシンボルはしばしば衝突します。
rioが、
[microlens: A tiny lens library with no dependencies](https://hackage.haskell.org/package/microlens)
のシンボルをre-exportしているけれど、
我々はしばしば本物の、
[lens: Lenses, Folds and Traversals](https://hackage.haskell.org/package/lens)
の方を使っているからです。

原則としてRIOのexportしているlensのシンボルだけで事足りる場合はそちらを使ってください。
もしlensのシンボルが必要であり、
lensをnamed importしてもシンボルが衝突してしまう場合は、
その時だけRIOのシンボルを`hiding`して回避してください。
