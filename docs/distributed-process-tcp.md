# distributed-processにおけるnetwork-transport-tcpのバイナリ表現

## 概要

distributed-processはHaskellで分散システムを構築するためのライブラリで、network-transport-tcpを使用してTCP上でメッセージのやり取りを行います。
このドキュメントでは、network-transport-tcpがどのようなバイナリ表現を使用してメッセージを送受信するかについて調査した結果をまとめます。

## network-transport-tcpの基本アーキテクチャ

### プロジェクトの概要

- **リポジトリ**: https://github.com/haskell-distributed/distributed-process (モノレポに入っている)
- **ライセンス**: BSD-3-Clause
- **言語**: Haskell (96.4%)

### 設計原則

1. **軽量接続**: エンドポイント間で数千の接続を作成しても問題ない軽量な接続
2. **明示的エラーハンドリング**: 例外を投げずに、型レベルでエラーを宣言
3. **抽象的エラーハンドリング**: 実装固有のエラーを汎用的なエラーにマッピング

## バイナリプロトコルの詳細

### メッセージシリアライゼーション

network-transport-tcpは`Data.Binary`パッケージを使用してメッセージのバイナリシリアライゼーションを行います：

```haskell
-- 例: TextMessageのバイナリインスタンス
data TextMessage = TextMessage Text
  deriving (Show, Generic)

instance Binary TextMessage
```

### バイナリ表現の特徴

1. **自動シリアライゼーション**: `Generic`と`Binary`インスタンスにより自動的にバイナリエンコーディングを生成
2. **標準エンコーディング**: Haskellの`Data.Binary`の標準的なエンコーディング形式を使用
3. **長さプレフィックス**: 一般的なバイナリプロトコルと同様に、メッセージの長さを示すプレフィックスを含む

### 接続管理プロトコル

#### 接続の多重化

- AとB間の複数の論理接続を単一のTCP接続にマッピング
- 最初の論理接続が確立されると同時にTCP接続を作成
- ConnectionIdは`HeavyweightConnectionId`と`LightweightConnectionId`の結合

#### 接続確立プロトコル

1. **接続要求**: AがBに接続を試行し、自身のエンドポイントアドレスを送信
2. **接続承認**: BがConnectionRequestAcceptedを送信して接続を確認
3. **クロス接続処理**: 相互接続要求の場合、辞書順でエンドポイントアドレスを比較して処理

### メッセージタイプ

#### 基本メッセージタイプ

- `ConnectionRequestAccepted`: 接続要求の承認
- `ConnectionRequestCrossed`: クロス接続の通知
- 実際のペイロードメッセージ

#### デバッグ情報

kaisui-distributed-processの実装では、以下のような詳細なバイナリ情報をログ出力：

```haskell
-- バイナリ表現の16進数表示
putStrLn $ "Binary representation (hex): " ++ hexStr
putStrLn $ "Binary length: " ++ show (L.length binaryMsg)
putStrLn $ "Message type: " ++ show msg
```

## 実装上の考慮事項

### エンディアンの扱い

- バイナリプロトコルでは、マルチバイト数値のエンディアン問題を考慮する必要がある
- Haskellの`Data.Binary`は標準的なエンディアン処理を行う

### エラーハンドリング

- TCP固有のエラー(「ソケット不足」など)を汎用エラー(「リソース不足」など)にマッピング
- 型安全なエラー処理により実行時例外を回避

### パフォーマンス最適化

- 軽量接続により大量の同時接続をサポート
- 効率的なバイナリエンコーディングによる低オーバーヘッド

## 比較: バイナリ vs テキストプロトコル

### バイナリプロトコルの利点

- **効率性**: 数値15は`00001111`として送信(4ビット)
- **パフォーマンス**: 機械読み取り用に最適化
- **帯域幅**: より少ないデータ量

### テキストプロトコルとの違い

- テキストプロトコルでは15は`'1'(00110001)+ '5'(00110101)`として送信(2バイト)
- 人間可読性はないが、機械処理には最適

## まとめ

network-transport-tcpは以下の特徴を持つバイナリプロトコルを実装しています：

1. **Data.Binaryベース**: Haskellの標準的なバイナリシリアライゼーション
2. **接続多重化**: 単一TCP接続上での複数論理接続
3. **明示的エラーハンドリング**: 型安全なエラー処理
4. **効率的設計**: 軽量接続と低オーバーヘッド
5. **自動エンコーディング**: Generic derivingによる自動バイナリ変換

このプロトコルにより、distributed-processは効率的で信頼性の高い分散システム通信を実現しています。
