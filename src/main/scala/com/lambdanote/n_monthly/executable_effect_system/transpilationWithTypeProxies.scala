package com.lambdanote.n_monthly.executable_effect_system

import cats.effect.IO
import org.http4s.client.Client

/**
 * 当ファイルについては、本文2.6.5項とternaryMixEliminationTypeclass.scalaを読んだ後に読むことを推奨する。
 *
 * この object transpilationWithTypeProxies の中では、ternaryMixEliminationTypeclass での明示的な命令型指定や、
 * .elaborate関数を利用する方法（本文2.6.5項）とはまた別の方法で
 * トランスパイラパイプライン内の型推論を支援する方法を紹介する。
 *
 * このファイルは def pipelineExplicit から読み進めると理解しやすいと思われる。
 * 実は、.elaborate関数を使わずに直接的に.transpile関数を使ってトランスパイラのパイプラインを記述した方が、
 * 本文（例えば、2.6.2節の図2.12）の説明に沿ったコードになる。
 * しかしその場合、命令セット型の指定を半手動で行う必要があるのだ、という問題を紹介するのが
 * pipelineExplicit 関数である。
 */
object transpilationWithTypeProxies {
  // Mixed[_] に対する型クラス制約で : AllowsElimOf[F] と書くための型エイリアス
  type AllowsElimOf[F[_]] = [Mixed[_]] =>> AllowsElim[Mixed, F]

  def elabAndElimHttpGet(httpClient: Client[IO])[Src[_]: AllowsElimOf[HttpGet] as elim](
    using icC: Containment[elim.ElimResult, IO],
    logC: Containment[elim.ElimResult, LogInstrs]
  ): [X] => Src[X] => Executable[elim.ElimResult, X] =
    [X] =>
      (instr: Src[X]) =>
        elim.classify(instr).inner match
          case Left(HttpGet.Request(url)) =>
            for {
              _ <- logC.subInstrAsExe(
                LogInstrs.Info(s"sending request to $url")
              )
              responseBody <- icC.subInstrAsExe(httpClient.expect[Array[Byte]](url))
              _ <- logC.subInstrAsExe(
                LogInstrs.Info(
                  s"received response of size ${responseBody.length}"
                )
              )
            } yield IArray.from(responseBody)
          case Right(others) => others.asSingleInstrExe

  def elabAndEliminateLog[Src[_]: AllowsElimOf[LogInstrs] as elim](
    using ioC: Containment[elim.ElimResult, IO]
  ): [X] => Src[X] => Executable[elim.ElimResult, X] =
    [X] =>
      (instr: Src[X]) =>
        elim.classify(instr).inner match
          case Left(LogInstrs.Info(msg))  => ioC.subInstrAsExe(IO.println(s"[INFO] $msg"))
          case Left(LogInstrs.Warn(msg))  => ioC.subInstrAsExe(IO.println(s"[WARN] $msg"))
          case Left(LogInstrs.Error(msg)) => ioC.subInstrAsExe(IO.println(s"[ERROR] $msg"))
          case Right(others)              => others.asSingleInstrExe

  def pipelineExplicit(httpClient: Client[IO])[A](
    exe: Executable[IO mix LogInstrs mix HttpGet, A]
  ) =
    // .transpile を elabAndElimHttpGet や elabAndEliminateLog でそのまま使おうとすると、
    // ソース命令型（elabAndElimHttpGet や elabAndEliminateLog の Src[_]）を明示的に指定しないといけない。
    // これを全て具体的に記述すると元の命令セット型のmix順に依存してしまい、
    // 命令セット型の変更に弱い、脆いトランスパイラパイプラインになってしまう。
    //
    // この問題を本文2.6.5項で紹介する .elaborate とは異なる方法で解決するのが、
    // この関数のすぐ下に定義されている .transpileTyped 拡張メソッドである。
    exe
      .transpile(elabAndElimHttpGet(httpClient)[IO mix LogInstrs mix HttpGet])
      .transpile(elabAndEliminateLog[IO mix LogInstrs])
      .compactify

  extension [F[_], A](exe: Executable[F, A])
    def transpileTyped[G[_]](
      // 命令を展開する規則。
      //
      // `{ type SourceInstr = F }` 型は一種の型プロキシである。
      // これはあくまでexeの命令型Fをexpand関数の中で（具体的にプログラマが繰り返すこと無く）
      // 名前で参照できるようにするためのものであり、この引数は実行時には何の有用なデータも持たない。
      //
      // 型推論器との相互作用の観点から必ずしも同じものとは言い切れないが、概念上は、`{ type SourceInstr = F }` は
      // Haskellの Data.Proxy（https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Proxy.html）や
      // Rust の PhantomData（https://doc.rust-lang.org/std/marker/struct.PhantomData.html）に相当するものと考えられる。
      expand: { type SourceInstr = F } => [X] => F[X] => Executable[G, X]
    ): Executable[G, A] =
      exe.transpile([X] => (instr: F[X]) => expand(new { type SourceInstr = F })(instr))

  def pipelineWithTypeProxies0(httpClient: Client[IO])[A](
    exe: Executable[IO mix LogInstrs mix HttpGet, A]
  ) =
    // 型推論を支援するために型プロキシを使うと、
    // 途中の命令型を明示的に指定せずとも、コンパイラが命令型を確定できるようになる。
    exe
      .transpileTyped(tProxy => elabAndElimHttpGet(httpClient)[tProxy.SourceInstr])
      .transpileTyped(tProxy => elabAndEliminateLog[tProxy.SourceInstr])
      .compactify

  def pipelineWithTypeProxies1(httpClient: Client[IO])[A](
    // 命令群のmix順が入れ替わっている場合でも transpileTyped ができる。
    // elabAndElimHttpGet の Src には型プロキシ prox を経由して命令型が伝搬されるので、
    // プログラマが "LogInstrs mix HttpGet mix IO" などと書き直さないで済む。
    exe: Executable[LogInstrs mix HttpGet mix IO, A]
  ) =
    exe
      .transpileTyped(tProxy => elabAndElimHttpGet(httpClient)[tProxy.SourceInstr])
      .transpileTyped(tProxy => elabAndEliminateLog[tProxy.SourceInstr])
      .compactify

  def pipelineWithTypeProxies2(httpClient: Client[IO])[A](
    // 命令群のmix順が pipelineWithTypeProxies0 や pipelineWithTypeProxies1 と異なっているが、
    // 同様に動作するという別の例。
    exe: Executable[HttpGet mix LogInstrs mix IO, A]
  ) =
    exe
      .transpileTyped(tProxy => elabAndElimHttpGet(httpClient)[tProxy.SourceInstr])
      .transpileTyped(tProxy => elabAndEliminateLog[tProxy.SourceInstr])
      .compactify
}
