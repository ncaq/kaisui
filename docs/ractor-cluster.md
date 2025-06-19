# ractor_cluster 通信プロトコル解析レポート

## 概要

`ractor_cluster`は、Rustで実装された分散アクターシステムであり、ノード間通信にProtocol Buffers (protobuf) v3を使用しています。
このプロトコルは、ErlangのOTP分散ノード機能にインスパイアされた設計となっており、複数のノード間でアクターの通信とクラスタリングを実現します。

## プロトコル基本情報

### 使用技術

- **シリアライゼーション形式**: Protocol Buffers (protobuf) v3
- **Rustシリアライゼーションライブラリ**: `prost`
- **型変換トレイト**: `BytesConvertable`
- **認証方式**: "magic cookie"メカニズム (Erlang分散プロトコル準拠)
- **トランスポート層**: TCP (TLSオプション付き)
- **パッケージバージョン**: ractor_cluster 0.15.6 (2024年6月時点)

### プロトコル名称

このプロトコルには公式な固有名称は付けられていません。ドキュメントやソースコードでは以下のように言及されています：

- "inter-node defined protocol"
- "ractor分散プロトコル"
- "ractor clusterプロトコル"

## プロトコル構成

### 1. プロトコル定義ファイル

ractor_clusterは4つの主要なprotoファイルでプロトコルを定義しています：

#### 1.1 meta.proto - ネットワークメッセージ基盤

```protobuf
message NetworkMessage {
  oneof message {
    auth.AuthenticationMessage auth = 1;
    node.NodeMessage node = 2;
    control.ControlMessage control = 3;
  }
}
```

全てのネットワークメッセージの最上位メッセージ型で、3種類のメッセージカテゴリを統合的に扱います。

#### 1.2 auth.proto - 認証プロトコル

Erlangの分散システムガイドに基づく認証ハンドシェイクを実装：

- **NameMessage**: ノード名、フラグ、接続文字列を含む初期接続メッセージ
- **ServerStatus**: ハンドシェイク状態 (OK/拒否/同時接続等)
- **Challenge**: サーバーからのチャレンジリクエスト
- **ChallengeReply**: クライアントのチャレンジ応答
- **ChallengeAck**: サーバーのチャレンジ確認

認証フローはMD5ダイジェストを使用した相互認証を採用しています。

#### 1.3 node.proto - アクター間通信

リモートアクター間の通信メッセージを定義：

- **Cast**: 一方向のメッセージ送信

    - 宛先アクターID
    - ペイロード (バイナリ)
    - バリアント名 (列挙型の判別子)
    - メタデータ

- **Call**: リモートプロシージャコール (RPC)

    - 宛先アクターID
    - 引数
    - タグ (リクエスト識別子)
    - タイムアウト
    - バリアント名
    - メタデータ

- **CallReply**: RPC応答
    - 宛先アクターID
    - タグ (対応するリクエストの識別子)
    - ペイロード

#### 1.4 control.proto - クラスター制御

ノード間の制御メッセージとライフサイクル管理：

- **Actor**: アクターの基本情報 (PID、名前)
- **Ping/Pong**: ハートビート通信
- **Spawn**: リモートアクター生成
- **Terminate**: アクター終了
- **PgJoin/PgLeave**: プロセスグループ参加/離脱

## シリアライゼーション仕様

### BytesConvertableトレイト

全てのメッセージ型は`BytesConvertable`トレイトを実装する必要があります：

```rust
pub trait BytesConvertable {
    fn from_bytes(bytes: Vec<u8>) -> Self;
    fn into_bytes(self) -> Vec<u8>;
}
```

### 派生マクロ

開発効率を高めるため、以下の派生マクロが提供されています：

- `RactorMessage`: ローカル専用メッセージ用
- `RactorClusterMessage`: ネットワーク対応メッセージ用

### プロトバフサポート

prostを使用する場合の自動実装マクロ：

```rust
ractor_cluster::derive_serialization_for_prost_type! {MyProtobufType}
```

### メッセージ構造

各メッセージは以下の構造でシリアライズされます：

1. **引数データ**: メッセージの実際のペイロード (バイト配列)
2. **バリアント名**: 列挙型メッセージの判別子 (文字列)
3. **RPCポート**: RPC呼び出し時の応答ポート情報 (該当時のみ)

## ネットワーク通信フロー

### 1. 接続確立フェーズ

1. **初期接続**: クライアントがサーバーのTCPポートに接続
2. **名前交換**: 両ノードが`NameMessage`を交換
3. **認証チャレンジ**: magic cookieを使用した相互認証
4. **接続確立**: 認証成功後、`NodeSession`アクターが生成される

### 2. メッセージ交換フェーズ

1. **メッセージエンコード**: `BytesConvertable`を使用してバイト配列に変換
2. **プロトバフラッピング**: `NetworkMessage`でラップ
3. **TCP送信**: 接続済みのストリームで送信
4. **受信とデコード**: 逆の手順でメッセージを復元

### 3. ライフサイクル管理

- **ハートビート**: 定期的なPing/Pongで接続維持
- **アクター管理**: RemoteActorの生成・終了を管理
- **グループ同期**: PGグループのメンバーシップを同期

## セキュリティ機能

### 認証

- **Magic Cookie**: Erlang準拠の共有秘密鍵による認証
- **MD5ダイジェスト**: チャレンジ・レスポンス認証での使用

### 暗号化

- **TLSサポート**: オプションでTLS暗号化接続が可能
- **接続API**:
    - `client_connect()`: 平文接続
    - `client_connect_enc()`: 暗号化接続

### 検証

- **双方向認証**: サーバー・クライアント両方が相互に認証
- **タイムアウト**: RPC呼び出しのタイムアウト機能

## 実装例 (kaisui-ractorプロジェクトより)

### シンプルなメッセージ型の実装

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextMessage(pub String);

impl BytesConvertable for TextMessage {
    fn from_bytes(bytes: Vec<u8>) -> Self {
        let text = String::from_utf8(bytes)
            .unwrap_or_else(|_| "Invalid UTF-8".to_string());
        TextMessage(text)
    }

    fn into_bytes(self) -> Vec<u8> {
        self.0.as_bytes().to_vec()
    }
}
```

### ネットワーク対応メッセージの定義

```rust
use ractor_cluster::RactorClusterMessage;
use ractor::RpcReplyPort;

#[derive(RactorClusterMessage)]
enum DistributedMessage {
    Cast1(String, u64),
    #[rpc]
    Call1(u8, i64, RpcReplyPort<Vec<String>>),
}
```

## 関連ファイルパス

- `ractor_cluster/src/protocol/`: protobuf定義ファイル群
    - `meta.proto`
    - `auth.proto`
    - `node.proto`
    - `control.proto`
- `ractor_cluster/src/protocol.rs`: 生成コードの統合モジュール
- `ractor_cluster/src/node.rs`: ノード実装
- `ractor_cluster/src/net.rs`: ネットワーク通信実装

## まとめ

ractor_clusterの通信プロトコルは、Erlangの実績ある分散システム設計を参考にしつつ、Rustの型安全性とProtocol Buffersの効率性を組み合わせた実装となっています。
プロトコル自体に固有の名称はありませんが、その設計は堅牢で柔軟な分散アクターシステムの構築を可能にしています。
