package zio.cli.examples

import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.console.{ putStrLn, Console }
import zio.{ App, ExitCode, URIO, ZIO }

import java.nio.file.Path

object ComplexApp extends App {

  sealed trait GitOptions
  final case class GitO(version: Boolean)         extends GitOptions
  case object GitStashO                           extends GitOptions
  final case class GitStashPushO(force: Boolean)  extends GitOptions
  case object GitStashClearO                      extends GitOptions
  final case class GitPushO(repo: Option[String]) extends GitOptions

  val git: Command[(GitOptions, Unit)] = Command("git", Options.bool("version", true).as(GitO), Args.Empty)

  val gitPush: Command[(GitOptions, ::[Path])] = Command(
    "push",
    Options.text("repo").optional("Use different repository").as(GitPushO),
    Args.file("files", Exists.Yes).repeat1
  )

  val gitStash: Command[(GitOptions, Unit)] = Command("stash", Options.Empty.as(GitStashO), Args.Empty)

  val gitStashPush: Command[(GitOptions, ::[Path])] = Command(
    "push",
    Options.bool("force", ifPresent = true).as(GitStashPushO),
    Args.file("files", Exists.Yes).repeat1
  )

  val gitStashClear: Command[(GitOptions, Unit)] =
    Command("clear", Options.Empty.as(GitStashClearO), Args.Empty)

  val gitStashFull: Command[((GitOptions, Unit), (GitOptions, Any))] = gitStash
    .subcommands(
      gitStashPush,
      gitStashClear
    )

  val gitFull: Command[((GitOptions, Unit), (Object, Serializable))] =
    git.subcommands(
      gitStash
        .subcommands(
          gitStashPush,
          gitStashClear
        ),
      gitPush
    )

  val execute: ((GitOptions, Unit), Any) => URIO[Console, Unit] = { (opts, args) =>
    val cmd = opts._1.toString

    putStrLn("") *> putStrLn(s"Running $cmd...") *> putStrLn(s"Opts: $opts") *> putStrLn(s"Args: $args")
  }

  val app = CliApp(
    "Complex",
    "0.0.0",
    text("Showcase complex command structure"),
    gitFull,
    execute.tupled
  )

  override def run(args: List[String]): ZIO[Console, Nothing, ExitCode] = app.run(args)
}
