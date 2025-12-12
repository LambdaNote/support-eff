package com.lambdanote.n_monthly.executable_effect_system

import cats.effect.IO
import cats.effect.Ref
import cats.effect.unsafe.IORuntime
import cats.Monad
import java.net.URL
import org.http4s.client.Client

// ------
// 2.3節のコード（AssemblyProgramからWFAssemblyProgramまで）

enum Reg:
  case A, B, C, D

object wfAssemblyProgram {
  class AssemblyProgram[Instr](
    val firstInstr: Instr,
    branchesThunk: => List[AssemblyProgram[Instr]]
  ) {
    lazy val branches = branchesThunk
  }

  // format: off
  enum IncDecEq0Exit:
    case Inc(reg: Reg)            // 単一の分岐先を持つインクリメント命令
    case Dec(reg: Reg)            // 単一の分岐先を持つデクリメント命令
    case BranchIfEqZero(reg: Reg) // 2つの分岐先を持つ分岐（=0ならば分岐する）命令
    case Exit(code: Int)          // ゼロ個の分岐先を持つ終了命令

  import IncDecEq0Exit.*

  val prog: AssemblyProgram[IncDecEq0Exit] =
    AssemblyProgram(BranchIfEqZero(Reg.B), List(
      AssemblyProgram(Inc(Reg.A), List(
        AssemblyProgram(Inc(Reg.A), List(
          AssemblyProgram(Dec(Reg.B), List(prog))
        ))
      )),
      AssemblyProgram(Exit(0), List())
    ))

  val illFormed: AssemblyProgram[IncDecEq0Exit] =
    AssemblyProgram(BranchIfEqZero(Reg.B), List(
      AssemblyProgram(Inc(Reg.A), List(
        AssemblyProgram(Exit(0), List(illFormed)),
        illFormed
      ))
    ))

  enum WFAssemblyProgram[Instr[_]]:
    case NonEmpty[I[_], BranchIndices](
      firstInstr: I[BranchIndices],
      branches: BranchIndices => WFAssemblyProgram[I]
    ) extends WFAssemblyProgram[I]

  enum IncDecEq0ExitWithBI[BranchIndex]:
    case Inc(reg: Reg) extends IncDecEq0ExitWithBI[Unit]                // インクリメント命令
    case Dec(reg: Reg) extends IncDecEq0ExitWithBI[Unit]                // デクリメント命令
    case BranchIfEqZero(reg: Reg) extends IncDecEq0ExitWithBI[Boolean]  // 分岐命令
    case Exit(code: Int) extends IncDecEq0ExitWithBI[Nothing]           // 終了命令

  // format: on
}

// ------
// 2.4節のコード（Executableからfor式によるプログラム構築まで）

enum Executable[Instr[_], ExitCode]:
  case Exit[I[_], EC](
    code: EC
  ) extends Executable[I, EC]

  case NonEmpty[I[_], BranchIndex, EC](
    headInstruction: I[BranchIndex],
    branches: BranchIndex => Executable[I, EC]
  ) extends Executable[I, EC]
import Executable.*

enum CompRes:
  case Less, Eq, Greater

enum IncDecCompInstr[B]:
  case Inc(reg: Reg) extends IncDecCompInstr[Unit]
  case Dec(reg: Reg) extends IncDecCompInstr[Unit]
  case Comp(reg: Reg, value: Int) extends IncDecCompInstr[CompRes]

val decRegAByTwoUntilNonPos: Executable[IncDecCompInstr, String] =
  Executable.NonEmpty(
    IncDecCompInstr.Dec(Reg.A),
    _ =>
      Executable.NonEmpty(
        IncDecCompInstr.Dec(Reg.A),
        _ =>
          Executable.NonEmpty(
            IncDecCompInstr.Comp(Reg.A, 0),
            {
              case CompRes.Less    => Executable.Exit("< 0 in the end")
              case CompRes.Eq      => Executable.Exit("== 0 in the end")
              case CompRes.Greater => decRegAByTwoUntilNonPos
            }
          )
      )
  )

extension [I[_], X](v: I[X])
  def asSingleInstrExe: Executable[I, X] =
    Executable.NonEmpty(v, branchIndex => Executable.Exit(branchIndex))

extension [F[_], A](exe: Executable[F, A])
  def replaceExits[B](
    replacementRule: A => Executable[F, B]
  ): Executable[F, B] =
    exe match
      case Executable.NonEmpty(head, rest) =>
        // すべての分岐先について再帰的に置換を行っていく
        Executable.NonEmpty(head, b => rest(b).replaceExits(replacementRule))
      case Executable.Exit(ec) =>
        // 置換個所（Exitノード）に到達したので置換する
        replacementRule(ec)

  def flatMap[B](replacementRule: A => Executable[F, B]): Executable[F, B] =
    exe.replaceExits(replacementRule)

  def map[B](f: A => B): Executable[F, B] =
    exe.replaceExits(a => Executable.Exit(f(a)))

val decRegAByTwoUntilNonPos1: Executable[IncDecCompInstr, String] =
  for {
    _ <- IncDecCompInstr.Dec(Reg.A).asSingleInstrExe
    _ <- IncDecCompInstr.Dec(Reg.A).asSingleInstrExe
    comp <- IncDecCompInstr.Comp(Reg.A, 0).asSingleInstrExe
    result <- comp match {
      case CompRes.Less    => Executable.Exit("< 0 in the end")
      case CompRes.Eq      => Executable.Exit("== 0 in the end")
      case CompRes.Greater => decRegAByTwoUntilNonPos1
    }
  } yield result

// ------
// 2.5節のコード（compactify/transpile関数の定義と、それらを用いたunsafeRunReadByteExeの例まで）

object readWrite_Example {
  enum ReadWriteByte[B]:
    case ReadByte extends ReadWriteByte[Byte]
    case WriteByte(b: Byte) extends ReadWriteByte[Unit]

  val rwByteTestProgram = {
    import Executable.*, ReadWriteByte.*
    NonEmpty(
      ReadByte,
      (inputByte: Byte) =>
        NonEmpty(WriteByte((inputByte + 1).toByte), _ => Exit(()))
    )
  }

  enum ReadWriteBigInt[B]:
    case ReadBigInt extends ReadWriteBigInt[BigInt]
    case WriteBigInt(b: BigInt) extends ReadWriteBigInt[Unit]

  val rwBigIntTestProgram = {
    import Executable.*, ReadWriteBigInt.*
    NonEmpty(
      ReadBigInt,
      (input: BigInt) =>
        NonEmpty(WriteBigInt(input + 1), _ => Exit(()))
    )
  }
}

extension [F[_], ExitCode](program: Executable[F, ExitCode])
  def transpile[G[_]](
    expand: [X] => F[X] => Executable[G, X] // 命令を展開する規則
  ): Executable[G, ExitCode] =
    program match
      case Exit(exitCode)                => Exit(exitCode) // 終了ノードはそのまま
      case NonEmpty(headInstr, branches) =>
        expand(headInstr).replaceExits(branchIndex =>
          branches(branchIndex).transpile(expand)
        )

object transpileExample {
  import readWrite_Example.*

  def expandAsRealReadWrite(
    cell: Ref[IO, Byte]
  ): [B] => ReadWriteByte[B] => Executable[IO, B] =
    [B] =>
      (instr: ReadWriteByte[B]) =>
        instr match
          case ReadWriteByte.ReadByte =>
            cell.get.asSingleInstrExe
          case ReadWriteByte.WriteByte(b) =>
            cell.set(b).asSingleInstrExe

  def unsafeRunReadByteExe[EC](
    prog: Executable[ReadWriteByte, EC]
  )(using rt: IORuntime): EC =
    val io: IO[EC] = for {
      cell <- Ref[IO].of[Byte](0) // Ref[IO, Byte]を0で初期化して用意
      compiled = prog.transpile(expandAsRealReadWrite(cell)).compactify
      runResult <- compiled
    } yield runResult

    io.unsafeRunSync()(using rt)
}

extension [E](program: Executable[IO, E])
  def compactifyIO: IO[E] =
    program match
      case Exit(ec)                      => IO.pure(ec)
      case NonEmpty(headInstr, branches) =>
        headInstr.flatMap(r => branches(r).compactifyIO)

extension [M[_]: Monad as m, E](program: Executable[M, E])
  def compactify: M[E] =
    program match
      case Exit(ec)                      => m.pure(ec)
      case NonEmpty(headInstr, branches) =>
        m.flatMap(headInstr)(r => branches(r).compactify)

// 2.5.4節の、compactifyに至る議論で出てくるコード群
object finiteFormalSum {
  case class FiniteIntSum(summands: List[Int])

  /**
   * モノイドの型クラスの定義。実装規約として、次の等式が守られることを要請する。
   *  - `combine(x, unit) = x`
   *  - `combine(unit, y) = y`
   *  - `combine(combine(x, y), z) = combine(x, combine(y, z))`
   */
  trait Monoid[T] {
    def unit: T
    def combine(x: T, y: T): T
  }

  // モノイド値のリストに対して呼び出せる拡張関数
  extension [M: Monoid as m](ms: List[M])
    def combineAll: M = ms.fold(m.unit)(m.combine)

  // インスタンス宣言
  given Monoid[FiniteIntSum]:
    def unit = FiniteIntSum(List())
    def combine(x: FiniteIntSum, y: FiniteIntSum): FiniteIntSum =
      FiniteIntSum(x.summands ++ y.summands)

  List(
    FiniteIntSum(List(1)),
    FiniteIntSum(List(3, 5)),
    FiniteIntSum(List(7, 9, 11))
  ).combineAll // FiniteIntSum(List(1, 3, 5, 7, 9, 11))

  case class FormalSum[T](summands: List[T])

  given [T] => Monoid[FormalSum[T]]:
    def unit = FormalSum(List())
    def combine(x: FormalSum[T], y: FormalSum[T]): FormalSum[T] =
      FormalSum(x.summands ++ y.summands)

  enum PrimaryColor:
    case Red, Blue, Green

  // RedとRedとRedとBlueを（この順番で）足す
  val rrrb = FormalSum(List(
    PrimaryColor.Red,
    PrimaryColor.Red,
    PrimaryColor.Red,
    PrimaryColor.Blue
  ))

  // 「Monoid[M]が与えられていれば、FormalSum[M]を評価できる」
  extension [M: Monoid as m](ms: FormalSum[M])
    def evaluate: M =
      // "using m"は
      // 「combineAllで使うMonoidインスタンスをmにせよ」
      // という指示
      ms.summands.combineAll(using m)

  // インスタンス宣言
  given Monoid[Int]:
    def unit = 0
    def combine(x: Int, y: Int): Int = x + y

  // "1 + 1 + 2"という式を、和をIntの+だと解釈することでInt値に評価する
  FormalSum(List(1, 1, 2)).evaluate // 4
}

// ------
// 2.6節のコード（mix型、Containment/AllowsElim型クラス、.elaborate関数）

case class Mix[F[_], G[_], A](inner: Either[F[A], G[A]])

// 中置記法で LogInstrs mix RandInstrs のように書けるようにする
infix type mix[F[_], G[_]] = [A] =>> Mix[F, G, A]

enum HttpGet[A]:
  // オペランドに指定されたURLにGetリクエストを送り、
  // レスポンスを最後まで読み切って、レスポンスのボディを
  // IArray[Byte]として返す
  case Request(url: String) extends HttpGet[IArray[Byte]]

enum LogInstrs[B]:
  case Info(msg: String) extends LogInstrs[Unit]
  case Warn(msg: String) extends LogInstrs[Unit]
  case Error(msg: String) extends LogInstrs[Unit]

enum RandInstrs[B]:
  case NextByte extends RandInstrs[Byte]
  case NextInt extends RandInstrs[Int]
  case NextLong extends RandInstrs[Long]

// 全ての命令を一度にIOに展開する形の（「モジュラーでない」が、妥当な）トランスパイラ
object onePassTranspilation {
  def expandHttpGetLogRand[B](httpClient: Client[IO])(
    instr: (HttpGet mix LogInstrs mix RandInstrs)[B]
  ): Executable[IO, B] =
    instr match
      case Mix(Left(Mix(Left(HttpGet.Request(url))))) =>
        (
          for {
            _ <- IO.println(s"[INFO] sending request to $url")
            responseBody <- httpClient.expect[Array[Byte]](url)
            _ <- IO.println(
              s"[INFO] received response of " +
                s"size ${responseBody.length}"
            )
          } yield IArray.from(responseBody)
        ).asSingleInstrExe
      case Mix(Left(Mix(Right(LogInstrs.Info(msg))))) =>
        IO.println(s"[INFO] $msg").asSingleInstrExe
      case Mix(Left(Mix(Right(LogInstrs.Warn(msg))))) =>
        IO.println(s"[WARN] $msg").asSingleInstrExe
      case Mix(Left(Mix(Right(LogInstrs.Error(msg))))) =>
        IO.println(s"[ERROR] $msg").asSingleInstrExe
      case Mix(Right(RandInstrs.NextByte)) =>
        IO { scala.util.Random().nextBytes(1).head }.asSingleInstrExe
      case Mix(Right(RandInstrs.NextInt)) =>
        IO { scala.util.Random().nextInt() }.asSingleInstrExe
      case Mix(Right(RandInstrs.NextLong)) =>
        IO { scala.util.Random().nextLong() }.asSingleInstrExe
}

trait Containment[Mixed[_], G[_]]:
  // 「F[_]がG[_]を包含しているのなら、各G[A]をF[A]に『アップキャスト』できるはず」
  def upcastInclusion[A](ga: G[A]): Mixed[A]

extension [I[_], A](instr: I[A])
  def inject[Mixed[_]: {Contains[I] as incl}]: Executable[Mixed, A] =
    incl.upcastInclusion(instr).asSingleInstrExe

extension [Mixed[_], F[_]](containment: Containment[Mixed, F])
  def subInstrAsExe[A](fa: F[A]): Executable[Mixed, A] =
    containment.upcastInclusion(fa).asSingleInstrExe

// 型クラス制約を Mixed[_]: Contains[F] のように書くための型エイリアス
type Contains[F[_]] = [Mixed[_]] =>> Containment[Mixed, F]

given containsSelf: [F[_]] => Containment[F, F]:
  def upcastInclusion[A](fa: F[A]): F[A] = fa

given containsMixLImmediate: [F[_], R[_]] => Containment[F mix R, F]:
  def upcastInclusion[A](fa: F[A]): (F mix R)[A] = Mix(Left(fa))

given containsMixRImmediate: [F[_], L[_]] => Containment[L mix F, F]:
  def upcastInclusion[A](fa: F[A]): (L mix F)[A] = Mix(Right(fa))

given containsMixLSubTree: [L[_], R[_], F[_]] => (lIncl: Containment[L, F])
    => Containment[L mix R, F]:
  def upcastInclusion[A](fa: F[A]): (L mix R)[A] =
    Mix(Left(lIncl.upcastInclusion(fa)))

given containsMixRSubTree: [L[_], R[_], F[_]] => (rIncl: Containment[R, F])
    => Containment[L mix R, F]:
  def upcastInclusion[A](fa: F[A]): (L mix R)[A] =
    Mix(Right(rIncl.upcastInclusion(fa)))

trait AllowsElim[Mixed[_], F[_]]:
  type ElimResult[X]
  def classify[A](mixed: Mixed[A]): (F mix ElimResult)[A]

given elimLeftLeaf: [F[_], R[_]] => AllowsElim[F mix R, F]:
  type ElimResult[X] = R[X]
  def classify[A](mixed: (F mix R)[A]): (F mix R)[A] = mixed

given elimRightLeaf: [L[_], F[_]] => AllowsElim[L mix F, F]:
  type ElimResult[X] = L[X]
  def classify[A](mixed: (L mix F)[A]): (F mix L)[A] = Mix(mixed.inner.swap)

given elimTransL: [L[_], R[_], F[_]] => (lElim: AllowsElim[L, F])
  => (AllowsElim[L mix R, F] { type ElimResult[X] = (lElim.ElimResult mix R)[X] }) =
  new AllowsElim[L mix R, F]:
    type ElimResult[X] = (lElim.ElimResult mix R)[X]
    def classify[A](mixed: (L mix R)[A]): (F mix ElimResult)[A] =
      mixed.inner match
        case Left(la) =>
          lElim.classify(la).inner match
            case Left(fa)  => Mix(Left(fa))
            case Right(lr) => Mix(Right(Mix(Left(lr))))
        case Right(ra) => Mix(Right(Mix(Right(ra))))

given elimTransR: [L[_], R[_], F[_]] => (rElim: AllowsElim[R, F])
  => (AllowsElim[L mix R, F] { type ElimResult[X] = (L mix rElim.ElimResult)[X] }) =
  new AllowsElim[L mix R, F]:
    type ElimResult[X] = (L mix rElim.ElimResult)[X]
    def classify[A](mixed: (L mix R)[A]): (F mix ElimResult)[A] =
      mixed.inner match
        case Left(la)  => Mix(Right(Mix(Left(la))))
        case Right(ra) =>
          rElim.classify(ra).inner match
            case Left(fa)  => Mix(Left(fa))
            case Right(rr) => Mix(Right(Mix(Right(rr))))

extension [Src[_], A](program: Executable[Src, A])
  def elaborate[I[_]](using elim: AllowsElim[Src, I])(
    expand: [X] => I[X] => Executable[elim.ElimResult, X]
  ): Executable[elim.ElimResult, A] =
    program.transpile([X] =>
      (instr: Src[X]) =>
        elim.classify(instr).inner match
          case Left(targetInstr)   => expand(targetInstr)
          case Right(uninterested) => uninterested.asSingleInstrExe
    )

// ------
// 2.7節のコード（.elaborateによるトランスパイラ実装と、実際に動かせるテストプログラム）

object elaborationTranspilation {
  def httpGetElaboration(httpClient: Client[IO])[
    Res[_]: {Contains[IO], Contains[LogInstrs]}
  ]: [X] => HttpGet[X] => Executable[Res, X] =
    [X] =>
      (instr: HttpGet[X]) =>
        instr match
          case HttpGet.Request(url) =>
            for {
              _ <- LogInstrs.Info(s"sending request to $url").inject[Res]
              responseBody <- httpClient.expect[Array[Byte]](url).inject[Res]
              _ <- LogInstrs.Info(s"received response of size ${responseBody.length}")
                .inject[Res]
            } yield IArray.from(responseBody)

  def logElaboration[Res[_]: Contains[IO]]
    : [X] => LogInstrs[X] => Executable[Res, X] =
    [X] =>
      (instr: LogInstrs[X]) =>
        instr match
          case LogInstrs.Info(msg)  => IO.println(s"[INFO] $msg").inject[Res]
          case LogInstrs.Warn(msg)  => IO.println(s"[WARN] $msg").inject[Res]
          case LogInstrs.Error(msg) => IO.println(s"[ERROR] $msg").inject[Res]

  def transpilerPipeline(httpClient: Client[IO])[A](
    exe: Executable[IO mix LogInstrs mix HttpGet, A]
  ) =
    exe
      // : Executable[IO mix LogInstrs mix HttpGet, A]
      .elaborate[HttpGet](httpGetElaboration(httpClient))
      // : Executable[IO mix LogInstrs, A]
      .elaborate[LogInstrs](logElaboration) // : Executable[IO, A]
}

object workingExample {
  type Instr[A] = (IO mix LogInstrs mix HttpGet)[A]

  val testProgram =
    for {
      time1 <- IO { System.currentTimeMillis() }.inject[Instr]
      _ <- LogInstrs.Info("Diff checking started").inject[Instr]

      res1 <- HttpGet.Request("https://www.example.com").inject[Instr]
      res2 <- HttpGet.Request("https://www.example.net").inject[Instr]
      hasDifference =
        (res1.length != res2.length) ||
          res1.zip(res2).exists { case (b1, b2) => b1 != b2 }

      time2 <- IO { System.currentTimeMillis() }.inject[Instr]
      _ <- LogInstrs.Info(s"Diff checking finished in ${time2 - time1} ms").inject[Instr]
    } yield hasDifference

  import elaborationTranspilation.*

  def unsafeRunTestProgram: Boolean =
    // 標準的なIOランタイムを使う、という宣言
    import cats.effect.unsafe.implicits.global

    // HTTPクライアントの準備
    val httpClient: Client[IO] = {
      import cats.effect.unsafe.implicits.global
      import org.http4s.ember.client.EmberClientBuilder
      import org.typelevel.log4cats.LoggerFactory
      import org.typelevel.log4cats.slf4j.Slf4jFactory

      given LoggerFactory[IO] = Slf4jFactory.create[IO]
      EmberClientBuilder.default[IO].build.allocated.unsafeRunSync()._1
    }

    val compiled: IO[Boolean] =
      transpilerPipeline(httpClient)(testProgram) // トランスパイル
        .compactify // コンパイル
    compiled.unsafeRunSync() // ランタイムを用いた実行
}

// ------
// 2.8.1節のコード

enum ManyWorlds[BI]:
  /**
   * 「宇宙全体を二股に分岐させる」命令。
   * true が返ってくる宇宙と false が返ってくるような宇宙が同時に発生することを期待する
   */
  case Fork extends ManyWorlds[Boolean]

  // 「いまいる宇宙全体を破壊する」終端命令
  case DestroyCurrent extends ManyWorlds[Nothing]

import ManyWorlds.*

def expandManyWorldsInstr: [X] => ManyWorlds[X] => Executable[List, X] =
  [A] =>
    (instr: ManyWorlds[A]) =>
      instr match
        case Fork           => List(false, true).asSingleInstrExe
        case DestroyCurrent => List().asSingleInstrExe

def compileManyWorldsExe[A](exe: Executable[ManyWorlds, A]): List[A] =
  // List は Monad インスタンスを持つので compactify 先になれる
  exe.transpile(expandManyWorldsInstr).compactify
