package com.lambdanote.n_monthly.executable_effect_system

import cats.effect.IO
import org.http4s.client.Client

/**
 * 2.6.4項冒頭で説明したように「`.transpile`周辺の型推論がところどころうまくいかず、
 * 手動で命令セット型を指定して回ることに」なる、命令群の減算の三項関係としての定式化について
 * 説明するファイル。
 */
object ternaryMixEliminationTypeclass {
  trait MixElimination[Mixed[_], F[_], Result[_]]:
    def classify[A](mixed: Mixed[A]): Either[F[A], Result[A]]

  // 結果型 Res[_] に対する型クラス制約で Res[_] : MixElimResultOf[Mixed, F] と書くための型エイリアス
  type MixElimResultOf[Mixed[_], F[_]] = [R[_]] =>> MixElimination[Mixed, F, R]

  // ------
  // 型クラスインスタンスの定義。
  // mixの左右どちらかにFがそのままある場合の基本ケース（elimResultLeafL + elimResultLeafR）と、
  // 左右どちらかの深い場所にFが表れる場合の再帰ケース（elimResultLSubTree + elimResultRSubTree）を定義する。

  given elimResultLeafL: [F[_], R[_]] => MixElimination[F mix R, F, R]:
    def classify[A](mixed: (F mix R)[A]): Either[F[A], R[A]] =
      mixed.inner

  given elimResultLeafR: [L[_], F[_]] => MixElimination[L mix F, F, L]:
    def classify[A](mixed: (L mix F)[A]): Either[F[A], L[A]] =
      mixed.inner.swap

  given elimResultLSubTree: [
    L[_],
    R[_],
    F[_],
    LElimed[_]: MixElimResultOf[L, F] as lEr
  ] => MixElimination[L mix R, F, LElimed mix R] =
    new MixElimination[L mix R, F, LElimed mix R]:
      def classify[A](mixed: (L mix R)[A]): Either[F[A], (LElimed mix R)[A]] =
        mixed.inner match
          case Left(la) =>
            lEr.classify(la) match
              case Left(fa)  => Left(fa)
              case Right(lr) => Right(Mix(Left(lr)))
          case Right(ra) => Right(Mix(Right(ra)))

  given elimResultRSubTree: [
    L[_],
    R[_],
    F[_],
    RElimed[_]: MixElimResultOf[R, F] as rEr
  ] => MixElimination[L mix R, F, L mix RElimed] =
    new MixElimination[L mix R, F, L mix RElimed]:
      def classify[A](mixed: (L mix R)[A]): Either[F[A], (L mix RElimed)[A]] =
        mixed.inner match
          case Left(la)  => Right(Mix(Left(la)))
          case Right(ra) =>
            rEr.classify(ra) match
              case Left(fa)  => Left(fa)
              case Right(rr) => Right(Mix(Right(rr)))

  // ------
  // elaborate関数群の定義例

  def elaborateHttpGet(httpClient: Client[IO])[
    M[_],
    Res[_]: {MixElimResultOf[M, HttpGet] as res,
      Contains[LogInstrs] as logC,
      Contains[IO] as ioC}
  ]: [X] => M[X] => Executable[Res, X] =
    [X] =>
      (instr: M[X]) =>
        res.classify(instr) match
          case Left(HttpGet.Request(url)) =>
            for {
              _ <- LogInstrs.Info(s"sending request to $url").inject[Res]
              responseBody <- httpClient.expect[Array[Byte]](url).inject[Res]
              _ <-
                LogInstrs.Info(s"received response of size ${responseBody.length}").inject[Res]
            } yield IArray.from(responseBody)
          case Right(others) => others.asSingleInstrExe

  def elaborateLog[
    M[_],
    Res[_]: {MixElimResultOf[M, LogInstrs] as res, Contains[IO]}
  ]: [X] => M[X] => Executable[Res, X] =
    [X] =>
      (instr: M[X]) =>
        res.classify(instr) match
          case Left(LogInstrs.Info(msg))  => IO.println(s"[INFO] $msg").inject[Res]
          case Left(LogInstrs.Warn(msg))  => IO.println(s"[WARN] $msg").inject[Res]
          case Left(LogInstrs.Error(msg)) => IO.println(s"[ERROR] $msg").inject[Res]
          case Right(others)              => others.asSingleInstrExe

  // トランスパイラパイプラインの記述例
  def pipelineTyped(httpClient: Client[IO])[A](
    exe: Executable[IO mix HttpGet mix LogInstrs, A]
  ) =
    // これでも所望の動作は達成できる。しかし、型引数をMixElimResultOf制約のみから推論するのが
    // Scala 3.7.4 での型推論アルゴリズムだと難しいようで、型引数を（ソース側の命令型含めて）
    // すべて明示的に指定しなければならない。
    exe
      .transpile(elaborateHttpGet(httpClient)[IO mix HttpGet mix LogInstrs, IO mix LogInstrs])
      .transpile(elaborateLog[IO mix LogInstrs, IO])
      .compactify
}
