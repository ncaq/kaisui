## 重要コマンド

### フォーマット

基本的にファイルはツールで自動フォーマットしています。

フォーマットコマンドは引数などを変更せず例示のまま実行してください。

HaskellコードやRustコードなど、
[treefmt-nix](https://github.com/numtide/treefmt-nix)が対応しているファイルは以下のコマンドでフォーマット出来ます。

```console
nix fmt
```

Haskellのモジュールを追加した時や削除した時は、
[cabal-gild](https://hackage.haskell.org/package/cabal-gild)によるフォーマットで変更を反映する必要があります。
cabal-gildも`nix fmt`で実行されます。

### ビルド

#### Haskell

```console
cabal build --disable-optimization --enable-tests all
```

### テスト

#### Haskell

```console
cabal test --disable-optimization --enable-tests all
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

### 優先ライブラリ

以下のライブラリを優先して使ってください。

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

特に他のIO系ライブラリよりunliftioを優先してください。

### convertible

[convertible: Typeclasses and instances for converting between types](https://hackage.haskell.org/package/convertible)パッケージの、
`convert`関数で汎用的な型変換を行っています。
`encodeUtf8`関数のような個別の関数よりこちらを優先してください。
