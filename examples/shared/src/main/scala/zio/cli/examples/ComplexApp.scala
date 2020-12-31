package zio.cli.examples

import zio.cli.Command.Invocation
import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.console.{ putStrLn, Console }
import zio.{ App, ExitCode, URIO, ZIO }

object ComplexApp extends App {

  val git =
    Command("git", Options.bool("version", true), Args.Empty).subcommands(
      Command("stash", Options.Empty, Args.Empty)
        .subcommands(
          Command("push", Options.bool("force", ifPresent = true), Args.file("files", Exists.Yes).repeat1),
          Command("clear", Options.Empty, Args.Empty)
        ),
      Command("push", Options.text("repo").optional("Use different repository"), Args.file("files", Exists.Yes).repeat1)
    )

  val execute: (Any, Any) => URIO[Console, Unit] = { (opts, args) =>
    putStrLn(s"Opts: $opts") *> putStrLn(s"Args: $args")
  }

  val app = CliApp(
    "Complex",
    "0.0.0",
    text("Showcase complex command structure"),
    git,
    execute.tupled
  )

  override def run(args: List[String]): ZIO[Console, Nothing, ExitCode] = app.runFull(Invocation(Nil, args))
}
