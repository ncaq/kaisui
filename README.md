# kaisui

## 概要

このプロジェクトkaisuiは、
HaskellとRustなどの異なる言語のプログラムを組み合わせる方法として、
C FFIなどのメモリ共有によるものではなく、
Erlang風のメッセージパッシングライブラリを用いて通信する方法を提供することを目指します。

## なぜメモリ共有をしたくないのか

C FFIなどを使ったシンプルなメモリ共有モデルは複数の言語やシステムが混ざると複雑になります。

例えばガベージコレクションは複数言語のシステムがプロセス内部に混ざっていることは基本的に前提にしていません。
ガベージコレクションによって回収されるべきではないリソースが回収されてしまうことがありますし、
逆にメモリリークが発生する可能性もあります。

またプロセスを一つにまとめてしまうと、どちらかのプログラムが異常終了した時に全体が異常終了してしまうリスクが高くなります。

メモリ共有をせずプロセスごと分離してしまうことでほとんどの問題を回避することができます。
回避できない問題はパフォーマンスが悪化することですが、それはある程度諦めることにします。
このプロジェクトではパフォーマンスより安定性や書き易さを重視します。

## なぜ既存のメッセージパッシングライブラリを使うのか

ゼロからライブラリを自分で書くのは全てを自由にできて魅力的ですが、
機能性などを考えるとあまり良いものはできなさそうです。

結局はいろいろな言語に対応しないといけないので、
ゼロから書いたとしてもその言語の標準ライブラリに対応しないといけないのは同じです。

## メッセージパッシングライブラリに求める要件

TCPなどのネットワーク通信機構を使って外部のアクターと通信が出来ることが必要です。

ネットワーク通信は外部のコンピュータと接続する必要は今のところありませんが、
接続のモデルを簡単に考えるためにポータビリティが高い通信方法を使いたいためです。

## 目標のプログラミング感覚

メッセージを送受信するアクターを書いているときは、
他の言語に接続しているとかをあまり意識せずに、
ほとんどそのライブラリ同士で通信しているのと似たような感覚で書けることを目指します。

その言語のアクターと外部のアクターを接続するときは、
TCP/IPなどのアドレスを指定して接続することは問題ありません。

C FFIなどのForeign Function Interfaceは直接は使わなくて良いように設計します。

## distributed-process

[distributed-process: Cloud Haskell: Erlang-style concurrency in Haskell](https://hackage.haskell.org/package/distributed-process)
ぐらいしか有力なライブラリがHaskellにはなさそうなので、
必然的にこれを使います。

[haskell-distributed/distributed-process: Cloud Haskell core libraries](https://github.com/haskell-distributed/distributed-process)
にあるdistributed-process系のライブラリは積極的に活用していきます。

バックエンドには、
[network-transport-tcp: TCP instantiation of Network.Transport](https://hackage.haskell.org/package/network-transport-tcp)
を使います。
TCPがある程度シンプルでフォーマット変換時に開発しやすいと思うからです。

[network-transport-zeromq: ZeroMQ backend for network-transport](https://hackage.haskell.org/package/network-transport-zeromq)
はZeroMQを使う思想は魅力的でしたが開発が放棄されているので諦めます。
将来的にTCPではなくZeroMQベースに対応する時にこれをforkしたり参考にする可能性はあります。

## ractor

Rust側のメッセージパッシングライブラリはたくさんあるようでしたが、
とりあえず、
[ractor - crates.io: Rust Package Registry](https://crates.io/crates/ractor)
を使います。

ネットワーク通信経由で動いて安定してそうであればなんでも良かったので、
その中でメジャーそうであまり奇をてらっていないものを選びました。

[ractor_cluster - crates.io: Rust Package Registry](https://crates.io/crates/ractor_cluster)
についてはまだプロダクションレディではないと書いてあるのでひとまずは使いません。
別に自前でTCPバックエンドを書いても安定性は保証されていないのは同じというかむしろ劣ると思いますが、
とりあえずはシンプルさとこちらのコードのコントロール可能性を優先します。
将来的には採用するかもしれません。
