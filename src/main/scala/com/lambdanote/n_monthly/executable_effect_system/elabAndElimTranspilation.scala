package com.lambdanote.n_monthly.executable_effect_system

import cats.effect.IO
import org.http4s.client.Client

object elabAndElimTranspilation {
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

  def elabAndEliminateLog[M[_]: AllowsElimOf[LogInstrs] as elim](
    using ioC: Containment[elim.ElimResult, IO]
  ): [X] => M[X] => Executable[elim.ElimResult, X] =
    [X] =>
      (instr: M[X]) =>
        elim.classify(instr).inner match
          case Left(LogInstrs.Info(msg))  => ioC.subInstrAsExe(IO.println(s"[INFO] $msg"))
          case Left(LogInstrs.Warn(msg))  => ioC.subInstrAsExe(IO.println(s"[WARN] $msg"))
          case Left(LogInstrs.Error(msg)) => ioC.subInstrAsExe(IO.println(s"[ERROR] $msg"))
          case Right(others)              => others.asSingleInstrExe

  def pipelineExplicit(httpClient: Client[IO])[A](
    exe: Executable[IO mix LogInstrs mix HttpGet, A]
  ) =
    exe
      .transpile(elabAndElimHttpGet(httpClient)[IO mix LogInstrs mix HttpGet])
      .transpile(elabAndEliminateLog[IO mix LogInstrs])
      .compactify

  extension [F[_], A](exe: Executable[F, A])
    def transpileTyped[G[_]](
      // 命令を展開する規則。 { type SourceInstr = F } は一種の型プロキシである。
      expand: { type SourceInstr = F } => [X] => F[X] => Executable[G, X]
    ): Executable[G, A] =
      exe.transpile([X] => (instr: F[X]) => expand(new { type SourceInstr = F })(instr))

  // 型推論を支援するために型プロキシ（https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Proxy.html）を使うと、
  // 途中の命令型を明示的に指定せずとも、コンパイラが命令型を確定できるようになる。どの順番で transpile しても良い。
  def pipelineWithTypeProxies0(httpClient: Client[IO])[A](
    exe: Executable[IO mix LogInstrs mix HttpGet, A]
  ) =
    exe
      .transpileTyped(prox => elabAndElimHttpGet(httpClient)[prox.SourceInstr])
      .transpileTyped(prox => elabAndEliminateLog[prox.SourceInstr])
      .compactify

  def pipelineWithTypeProxies1(httpClient: Client[IO])[A](
    // 命令群のmix順が入れ替わっている場合でも transpileTyped ができる。
    // elabAndElimHttpGet の Src には型プロキシ prox を経由して命令型が伝搬されるので、
    // プログラマが "LogInstrs mix HttpGet mix IO" などと書き直さないで済む。
    exe: Executable[LogInstrs mix HttpGet mix IO, A]
  ) =
    exe
      .transpileTyped(prox => elabAndElimHttpGet(httpClient)[prox.SourceInstr])
      .transpileTyped(prox => elabAndEliminateLog[prox.SourceInstr])
      .compactify

  def pipelineWithTypeProxies2(httpClient: Client[IO])[A](
    exe: Executable[HttpGet mix LogInstrs mix IO, A]
  ) =
    exe
      .transpileTyped(prox => elabAndElimHttpGet(httpClient)[prox.SourceInstr])
      .transpileTyped(prox => elabAndEliminateLog[prox.SourceInstr])
      .compactify
}
