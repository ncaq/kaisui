## 重要コマンド

### フォーマット

基本的にファイルはツールで自動フォーマットしています。

フォーマットコマンドは引数などを変更せず例示のまま実行してください。

Haskellコードなど、
[treefmt-nix](https://github.com/numtide/treefmt-nix)が対応しているファイルは以下のコマンドでフォーマット出来ます。

```
nix fmt
```

Haskellのモジュールを追加した時や削除した時は、
[cabal-gild](https://hackage.haskell.org/package/cabal-gild)によるフォーマットで変更を反映する必要があります。
cabal-gildも`nix fmt`で実行されます。

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
  OverloadedStrings
  PartialTypeSignatures
  QuasiQuotes
  RecordWildCards
  StrictData
  TemplateHaskell
  TypeFamilies
  ViewPatterns
```

### 代替Preludeのrio

代替Preludeとして[rio: A standard library for Haskell](https://hackage.haskell.org/package/rio)を採用しています。

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

RIOが直接exportしている範囲では足りない場合は、
RIOが依存していて既にプロジェクトで間接的に依存している以下のパッケージを優先して使ってください。

- bytestring
- containers
- deepseq
- directory
- exceptions
- filepath
- hashable
- mtl
- primitive
- text
- time
- typed-process
- unliftio
- unliftio-core
- unordered-containers
- vector
- unix

他のIO系ライブラリよりunliftioを優先してください。

### convertible

[convertible: Typeclasses and instances for converting between types](https://hackage.haskell.org/package/convertible)パッケージの、
`convert`関数で汎用的な型変換を行っています。
`encodeUtf8`関数のような個別の関数よりこちらを優先してください。
