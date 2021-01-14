package zio.cli

import zio._
import zio.cli.figlet.FigFont
import zio.cli.HelpDoc.Span.{ code, text }
import zio.cli.Command.{ BuiltIn, Invocation }
import zio.cli.HelpDoc.{ h1, p }
import zio.console._

/**
 * A `CliApp[R, E]` is a complete description of a command-line application, which
 * requires environment `R`, and may fail with a value of type `E`.
 */
final case class CliApp[-R, +E, Model](
  name: String,
  version: String,
  summary: HelpDoc.Span,
  command: Command[Model],
  execute: Model => ZIO[R, E, Any],
  footer: HelpDoc = HelpDoc.Empty,
  config: CliConfig = CliConfig.default,
  figFont: FigFont = FigFont.Default
) { self =>
  def handleBuiltIn(invocation: Invocation, builtIn: BuiltIn): ZIO[Console, HelpDoc, Invocation] =
    if (builtIn.wizard) wizard
    else if (invocation.optsArgs.isEmpty || builtIn.help)
      printDocs(helpDoc).as(invocation)
    else
      builtIn.shellCompletions match {
        case None        => URIO.succeedNow(invocation)
        case Some(value) => putStrLn(completions(value)).as(invocation)
      }

  def completions(shellType: ShellType): String = ???

  def footer(f: HelpDoc): CliApp[R, E, Model] =
    copy(footer = self.footer + f)

  def helpDoc: HelpDoc =
    p(code(figFont.render(command.names.headOption.getOrElse(name)))) +
      p(text(name) + text(" ") + text(version) + text(" -- ") + summary) +
      h1("synopsis") +
      command.synopsis.helpDoc +
      command.helpDoc +
      footer

  def config(o: CliConfig): CliApp[R, E, Model] =
    copy(config = o)

  def run(args: List[String]): ZIO[R with Console, Nothing, ExitCode] =
    (for {
      allPaths                 <- command.paths
      baseInvocation           <- commandPath(allPaths, args)
      builtInValidationResult  <- command.parseBuiltIn(baseInvocation.optsArgs, config)
      (remainingArgs, builtIn) = builtInValidationResult
      inv                      <- handleBuiltIn(baseInvocation.copy(optsArgs = remainingArgs), builtIn)
      validationResult         <- command.parseInvocation(inv, config)
    } yield validationResult)
      .foldM(printDocs, success => execute(success._2))
      .exitCode

  def commandPath(paths: List[List[String]], call: List[String]): URIO[Console, Invocation] =
    URIO
      .succeedNow(paths)
      .map(_.sortBy(-_.length))
      .flatMap(x => ZIO.fromOption(x.find(call.startsWith(_))))
      .map(c => call.splitAt(c.length))
      .map { case (cmdPath, argsOpts) => Invocation(cmdPath, argsOpts) }
      .orElseSucceed(Invocation(Nil, call))

  def printDocs(helpDoc: HelpDoc): URIO[Console, Unit] =
    putStrLn(helpDoc.toPlaintext(80))

  def summary(s: HelpDoc.Span): CliApp[R, E, Model] =
    copy(summary = self.summary + s)

  private def wizard: ZIO[Console, HelpDoc, Invocation] =
    putStrLn("=" * 100) *>
      putStrLn(s"Welcome to $name wizard. Please select a command.") *>
      command.wizard.mapError(e => HelpDoc.h1("Something went wrong.") + HelpDoc.p(e.getMessage)) >>=
      dryRun

  private def dryRun(invocation: Invocation): URIO[Console, Invocation] =
    putStrLn("=" * 100) *>
      putStrLn(
        HelpDoc
          .p("You may bypass the wizard and execute your command directly with the following options and arguments:")
          .toPlaintext()
      ) *>
      putStrLn(invocation.mkString) *>
      URIO.succeed(invocation) <*
      putStrLn("=" * 100)
}
