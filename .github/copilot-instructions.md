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

### named importやqualified importを控える

必要が無いときはnamed importやqualified importは控えてください。
今はhaskell-language-serverがあるので、
そのlensでどれが何をimportしているのかは簡単に把握できるので、
コードが複雑に見えて美しくありません。

もちろん命名がコンフリクトした場合は適切に適度に使用してください。

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
素直に例外を投げてしまうの良いでしょう。

`IO`的な操作をしているが`IO`そのものではないモナドの場合は、
`MonadThrow`や`MonadIO`の型クラスが役に立つ場合があります。

ただし例外があり、
データベースを`lookup`するような操作は、
存在しないというデータが正常系として扱われるので、
その場合は`IO (Maybe a)`のようなシグネチャを使うことは適切です。

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

`makeFieldsId`Template Haskell関数を使ってレコードのフィールドアクセサを定義するときは、
フィールドにプレフィクスやアンダースコアは付けないでください。
`NoFieldSelectors`拡張の力でプレフィクスは不要になっています。
`makeFieldsId`関数は完全にフィールド名と同じアクセサを生成します。
プレフィクスやアンダースコアをつけると奇妙なアクセサが生成されてしまいます。

`makeFieldsId`関数を使うときは以下のようにimportを行ってください。

```haskell
import Control.Lens (makeFieldsId)
```

`Control.Lens`は`RIO`のexportしているシンボルと衝突しやすいからです。

その他のシンボルが必要になってからシンボルのimportを追加してください。
RIOと衝突したときは原則としてRIO側のシンボルを優先してください。
不要なときはimportしないでください。
