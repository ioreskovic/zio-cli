package zio.cli

import zio._
import zio.console._

object Wizard {
  def readInput: ZIO[Console, Throwable, List[String]] =
    getStrLn.map(in => List(in.trim).filterNot(_.isBlank))

  def prompt(synopsis: UsageSynopsis): ZIO[Console, Throwable, List[String]] =
    show(synopsis) *>
      putStr("Enter your choice: ") *>
      readInput <*
      putStrLn("")

  def show(synopsis: UsageSynopsis): URIO[Console, Unit] =
    putStrLn("-" * 100) *>
      putStrLn(synopsis.helpDoc.toPlaintext())
}
