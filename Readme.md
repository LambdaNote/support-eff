# 「作りながら考えるエフェクトシステム」（「n月刊ラムダノート Vol.5, No.2」収録）サポートサイト

[n月刊ラムダノート Vol.5, No.2](https://www.lambdanote.com/products/n-vol-5-no-2) 収録の「作りながら考えるエフェクトシステム（近藤亮介／Kory）」で紹介しているエフェクトシステムの実装を公開しています。

このレポジトリはsbtプロジェクトになっており、以下の3つの`.scala`ファイルを含んでいます。

- [`core.scala`](./src/main/scala/com/lambdanote/n_monthly/executable_effect_system/core.scala): 本文に現れる定義をそのまま単一のファイルに並べたもの
- [`ternaryMixEliminationTypeclass.scala`](./src/main/scala/com/lambdanote/n_monthly/executable_effect_system/ternaryMixEliminationTypeclass.scala): 2.6.4項冒頭で説明した「動きはするが型推論上の不都合がある」三項型クラスの実装
- [`transpilationWithTypeProxies.scala`](./src/main/scala/com/lambdanote/n_monthly/executable_effect_system/transpilationWithTypeProxies.scala): `.elaborate`ではなく`.transpile`を直接使う方針を貫き通すトランスパイラの実装例であって、型プロキシを用いることで型推論を補助するもの
  - このファイルのみ、本文中からは言及されていません。オマケ的な存在です。

## コード例の動かし方

### Dev Containerを用いる場合の手順（推奨）

コード例を動かすための環境を構築するためのDev Container定義が[`.devcontainer`](./.devcontainer)ディレクトリに準備されているので、
これを用いてコード例を動かす方法です。

- [VSCodeのセットアップ手順](https://code.visualstudio.com/docs/devcontainers/containers#_installation)に従って、DockerとVSCode、Dev Containers拡張機能をインストールする
- VSCodeのコマンドパレット（`F1`）から`Dev Containers: Clone Repository in Container Volume...`を実行し、レポジトリ名に`LambdaNote/support-eff`を入力する
- `/workspace`ディレクトリに居る状態で`sbt console`を実行してREPLコンソールに入る
- 次のように`import`文の後に`workingExample.unsafeRunTestProgram`を実行することで、コード2.67のコード片が実行できる。
  ```Scala
  scala> import com.lambdanote.n_monthly.executable_effect_system.*

  scala> workingExample.unsafeRunTestProgram
  [INFO] Diff checking started
  [INFO] sending request to https://www.example.com
  [INFO] received response of size 513
  [INFO] sending request to https://www.example.net
  [INFO] received response of size 513
  [INFO] Diff checking finished in 1118 ms
  val res0: Boolean = false
  ```

### 環境を自前で用意する場合の手順

- JDK 21（推奨：[Eclipse Adoptium Temurin JDK](https://adoptium.net/temurin/releases) 21）をインストールする
- [sbtの公式ドキュメントのインストール手順](https://www.scala-sbt.org/1.x/docs/ja/Setup.html)に従って`sbt`をインストールする
- 当レポジトリをcloneして、レポジトリディレクトリ内で`sbt console`を実行してREPLコンソールに入る
- 次のように`import`文の後に`workingExample.unsafeRunTestProgram`を実行することで、コード2.67のコード片が実行できる。
  ```Scala
  scala> import com.lambdanote.n_monthly.executable_effect_system.*

  scala> workingExample.unsafeRunTestProgram
  [INFO] Diff checking started
  [INFO] sending request to https://www.example.com
  [INFO] received response of size 513
  [INFO] sending request to https://www.example.net
  [INFO] received response of size 513
  [INFO] Diff checking finished in 1258 ms
  val res0: Boolean = false
  ```

環境によっては`SSLHandshakeException`が出るようなので、その場合、
`https://example.com`などのURLのスキームを`http`に差し替えれば動くはずです。
