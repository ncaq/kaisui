# ractor_cluster のバイナリプロトコル調査結果

## 概要

`ractor_cluster` は Rust の分散アクターシステムで、Protocol Buffers (protobuf) を使用してノード間通信を行います。ErlangのOTPの分散ノード機能にインスパイアされた設計となっており、複数のノード間でアクターの通信とクラスタリングを実現します。

## プロトコル構成

### 使用技術

- **プロトコル**: Protocol Buffers (protobuf) v3
- **シリアライゼーション**: `BytesConvertable` トレイトによる型変換
- **認証**: "magic cookie"メカニズム (Erlang分散プロトコル準拠)
- **パッケージバージョン**: ractor_cluster 0.15.6

### プロトコル定義ファイル

ractor_clusterは4つの主要なprotoファイルでプロトコルを定義しています:

#### 1. meta.proto - ネットワークメッセージ基盤

```protobuf
message NetworkMessage {
  oneof message {
    auth.AuthenticationMessage auth = 1;
    node.NodeMessage node = 2;
    control.ControlMessage control = 3;
  }
}
```

全てのネットワークメッセージの最上位メッセージ型で、3種類のメッセージを統合的に扱います。

#### 2. auth.proto - 認証プロトコル

Erlangの分散システムガイドに基づく認証ハンドシェイクを実装:

- **NameMessage**: ノード名、フラグ、接続文字列
- **ServerStatus**: ハンドシェイク状態 (OK/拒否/同時接続等)
- **Challenge**: サーバーからのチャレンジリクエスト
- **ChallengeReply**: クライアントのチャレンジ応答
- **ChallengeAck**: サーバーのチャレンジ確認

認証フローはMD5ダイジェストを使用した相互認証を採用しています。

#### 3. node.proto - アクター間通信

リモートアクター間の通信メッセージを定義:

- **Cast**: 一方向のペイロード送信
    - 宛先アクターID、ペイロード、バリアント、メタデータ
- **Call**: リモートプロシージャコール (RPC)
    - 宛先アクターID、引数、タグ、タイムアウト、バリアント、メタデータ
- **CallReply**: RPC応答
    - 宛先アクターID、タグ、ペイロード

#### 4. control.proto - クラスター制御

ノード間の制御メッセージとライフサイクル管理:

- **Actor**: アクターの基本情報(PID、名前)
- **Ping/Pong**: ハートビート通信
- **Spawn**: アクター生成
- **Terminate**: アクター終了
- **PgJoin/PgLeave**: プロセスグループ参加/離脱

## バイナリメッセージ形式

### シリアライゼーション要件

1. **BytesConvertableトレイト**: 全てのメッセージ型は `BytesConvertable` トレイトを実装する必要があります

    ```rust
    pub trait BytesConvertable {
        fn from_bytes(bytes: Vec<u8>) -> Self;
        fn into_bytes(self) -> Vec<u8>;
    }
    ```

2. **派生マクロ**:

    - `RactorMessage`: ローカル専用メッセージ
    - `RactorClusterMessage`: ネットワーク対応メッセージ

3. **プロトバフサポート**: prost使用時の自動実装マクロ
    ```rust
    ractor_cluster::derive_serialization_for_prost_type! {MyProtobufType}
    ```

### メッセージ構造

各メッセージは以下の構造でシリアライズされます:

- **バイト配列**: 引数のシリアライズデータ
- **バリアント名**: 列挙型の判別子
- **RPCポート**: RPC呼び出し時の応答ポート (該当時のみ)

### 実装例 (kaisui-ractorプロジェクトから)

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextMessage(pub String);

impl BytesConvertable for TextMessage {
    fn from_bytes(bytes: Vec<u8>) -> Self {
        let text = String::from_utf8(bytes).unwrap_or_else(|_| "Invalid UTF-8".to_string());
        TextMessage(text)
    }

    fn into_bytes(self) -> Vec<u8> {
        self.0.as_bytes().to_vec()
    }
}
```

## ネットワーク通信フロー

1. **認証フェーズ**: magic cookieを使用したノード認証
2. **コネクション確立**: TLSベースの安全な接続
3. **メッセージ交換**: protobufでシリアライズされたメッセージの送受信
4. **制御メッセージ**: ハートビートとライフサイクル管理

## セキュリティ特徴

- **認証**: Erlangの"magic cookie"メカニズム
- **暗号化**: TLS接続によるネットワーク通信の保護
- **検証**: MD5ダイジェストによる相互認証

## 制限事項

- プロダクション環境での使用は推奨されていません (開発チームによる表明)
- ストレステストが不十分
- Windows環境での自動ビルドに制約あり (protobuf-src依存関係)

## 関連ファイル

- `ractor_cluster/src/protocol/`: protobuf定義ファイル群
- `ractor_cluster/src/protocol.rs`: 生成コードの統合モジュール
- `ractor_cluster/src/node.rs`: ノード実装 (21,755バイト)
- `ractor_cluster/src/net.rs`: ネットワーク通信実装

このプロトコル仕様により、ractor_clusterは堅牢で柔軟な分散アクターシステムを実現しています。
