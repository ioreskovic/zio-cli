package zio.cli

import zio.cli.HelpDoc.{ h1, Span }
import zio.console.{ getStrLn, putStr, putStrLn, Console }
import zio.{ IO, URIO, ZIO }

/**
 * A `Command` represents a command in a command-line application. Every command-line application
 * will have at least one command: the application itself. Other command-line applications may
 * support multiple commands.
 */
sealed trait Command[+A] { self =>
  import Command._

  final def |[A1 >: A](that: Command[A1]): Command[A1] = Command.Fallback(self, that)

  final def as[B](b: => B): Command[B] = self.map(_ => b)

  def helpDoc: HelpDoc

  final def map[B](f: A => B): Command[B] = Command.Map(self, f)

  final def orElse[A1 >: A](that: Command[A1]): Command[A1] = self | that

  final def orElseEither[B](that: Command[B]): Command[Either[A, B]] = self.map(Left(_)) | that.map(Right(_))

  def parse(args: List[String], conf: CliConfig): IO[HelpDoc, (List[String], A)]

  final def subcommands[B](that: Command[B]): Command[(A, B)] = Command.Subcommands(self, that)

  final def subcommands[B](c1: Command[B], c2: Command[B], cs: Command[B]*): Command[(A, B)] =
    subcommands(cs.foldLeft(c1 | c2)(_ | _))

  private[cli] def foldSingle[C](initial: C)(f: (C, Single[_, _]) => C): C = self match {
    case c: Command.Single[o, a]      => f(initial, c)
    case map: Command.Map[a, b]       => map.command.foldSingle(initial)(f)
    case Fallback(left, right)        => right.foldSingle(left.foldSingle(initial)(f))(f)
    case s: Command.Subcommands[a, b] => s.child.foldSingle(s.parent.foldSingle(initial)(f))(f)
  }

  def synopsis: UsageSynopsis

  lazy val builtInOptions: Options[BuiltIn] =
    (Options.bool("help", ifPresent = true) :: Options.bool("wizard", ifPresent = true) :: ShellType.option.optional(
      "N/A"
    )).as(BuiltIn)

  final def parseBuiltIn(args: List[String], conf: CliConfig): IO[HelpDoc, (List[String], BuiltIn)] =
    builtInOptions.validate(args, conf)

  def wizard: ZIO[Console, Throwable, InvocationContext] =
    showCommands()
      .flatMap(max => selectCommand(max.ord))
      .flatMap(sel => wizardInternal(sel)(1))

  private[cli] def showCommands(ctx: ShowContext = ShowContext()): URIO[Console, ShowContext] = self match {
    case c: Command.Single[o, a] => {
      val ctx1 = ctx ~ c.name
      putStrLn(ctx1.toString).as(ctx1)
    }
    case Command.Map(command, _) => command.showCommands(ctx)
    case Command.Fallback(left, right) =>
      left.showCommands(ctx).flatMap(cl => right.showCommands(cl.next.copy(path = ctx.path)))
    case s: Command.Subcommands[a, b] =>
      s.parent.showCommands(ctx).flatMap(cl => s.child.showCommands(cl.next))
  }

  private[cli] def selectCommand(max: Int): ZIO[Console, Throwable, Int] =
    putStr(s"Please select command: ") *>
      getStrLn
        .flatMap(in => ZIO.effect(in.toInt))
        .orElseFail(new IllegalArgumentException("Not a valid command identifier"))
        .filterOrFail(i => i >= 1 && i <= max)(
          new IllegalArgumentException(s"Command selection not in range [1, $max].")
        )

  private[cli] def wizardInternal(target: Int)(current: Int): ZIO[Console, Throwable, InvocationContext]

}

object Command {
  final case class BuiltIn(help: Boolean, wizard: Boolean, shellCompletions: Option[ShellType])
  final case class NonRequestedSelection(idx: Int, cmdPath: List[String] = Nil) extends Throwable

  final case class ShowContext(ord: Int = 1, path: Vector[String] = Vector.empty) {
    def set(x: Int): ShowContext       = ShowContext(x, path)
    def next: ShowContext              = ShowContext(ord + 1, path)
    def prev: ShowContext              = ShowContext(ord - 1, path)
    def ~(seg: String): ShowContext    = ShowContext(ord, path :+ seg)
    override lazy val toString: String = String.format("[%2d] %s", ord, path.mkString(" "))
  }

  final case class InvocationContext(commandPath: List[String], optsArgs: List[String]) {
    def :+(seg: String): InvocationContext             = InvocationContext(commandPath :+ seg, optsArgs)
    def +:(seg: String): InvocationContext             = InvocationContext(seg +: commandPath, optsArgs)
    def ++:(segments: List[String]): InvocationContext = InvocationContext(segments ::: commandPath, optsArgs)
    lazy val full: List[String]                        = commandPath ::: optsArgs
    override lazy val toString: String                 = full.mkString(" ")
  }

  final case class Single[OptionsType, ArgsType](
    name: String,
    description: HelpDoc,
    options: Options[OptionsType],
    args: Args[ArgsType]
  ) extends Command[(OptionsType, ArgsType)] { self =>
    def helpDoc: HelpDoc = {
      val descriptionsSection = {
        val desc = description

        if (desc.isEmpty) HelpDoc.Empty
        else h1("description") + desc
      }

      val argumentsSection = {
        val args = self.args.helpDoc

        if (args == HelpDoc.Empty) HelpDoc.Empty
        else h1("arguments") + self.args.helpDoc
      }

      val optionsSection = {
        val opts = (self.options :: self.builtInOptions).helpDoc

        if (opts == HelpDoc.Empty) HelpDoc.Empty
        else h1("options") + opts
      }

      descriptionsSection + argumentsSection + optionsSection
    }

    final def parse(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, (List[String], (OptionsType, ArgsType))] =
      for {
        tuple               <- self.options.validate(args, conf)
        (args, optionsType) = tuple
        tuple               <- self.args.validate(args, conf)
        (args, argsType)    = tuple
        _ <- ZIO.when(args.nonEmpty)(
              ZIO.fail(HelpDoc.p(Span.error(s"Unexpected arguments for command ${name}: ${args}")))
            )
      } yield (args, (optionsType, argsType))

    def synopsis: UsageSynopsis =
      UsageSynopsis.Named(name, None) + options.synopsis + args.synopsis

    override def wizardInternal(target: Int)(current: Int): ZIO[Console, Throwable, InvocationContext] =
      ZIO.when(current != target)(ZIO.fail(NonRequestedSelection(current, List(name)))) *>
        putStrLn("=" * 100) *>
        putStrLn(s"Selected command: $name") *>
        Wizard.show(synopsis) *>
        options.wizard.zipWith(args.wizard)(_ ++ _).map(InvocationContext(List(name), _))
  }

  final case class Map[A, B](command: Command[A], f: A => B) extends Command[B] {
    def helpDoc = command.helpDoc

    final def parse(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, (List[String], B)] = command.parse(args, conf).map {
      case (leftover, a) => (leftover, f(a))
    }

    def synopsis: UsageSynopsis = command.synopsis

    override def wizardInternal(target: Int)(current: Int): ZIO[Console, Throwable, InvocationContext] =
      command.wizardInternal(target)(current)
  }

  final case class Fallback[A](left: Command[A], right: Command[A]) extends Command[A] {
    def helpDoc = left.helpDoc + right.helpDoc

    final def parse(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, (List[String], A)] = left.parse(args, conf) orElse right.parse(args, conf)

    def synopsis: UsageSynopsis = UsageSynopsis.Mixed

    override def wizardInternal(target: Int)(current: Int): ZIO[Console, Throwable, InvocationContext] =
      left
        .wizardInternal(target)(current)
        .catchSome { case NonRequestedSelection(idx, _) => right.wizardInternal(target)(idx + 1) }
  }

  final case class Subcommands[A, B](parent: Command[A], child: Command[B]) extends Command[(A, B)] {
    def helpDoc = parent.helpDoc + h1("subcommands") + child.helpDoc

    final def parse(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, (List[String], (A, B))] = parent.parse(args, conf).flatMap {
      case (leftover, a) => child.parse(leftover, conf).map(t => (t._1, (a, t._2)))
    }

    def synopsis: UsageSynopsis = parent.synopsis

    override def wizardInternal(target: Int)(current: Int): ZIO[Console, Throwable, InvocationContext] =
      parent
        .wizardInternal(target)(current)
        .catchSome {
          case NonRequestedSelection(idx, cp) =>
            child.wizardInternal(target)(idx + 1).map(ctx => cp ++: ctx)
        }
  }

  /**
   * Construct a new command.
   */
  def apply[OptionsType, ArgsType](
    name: String,
    options: Options[OptionsType],
    args: Args[ArgsType],
    helpDoc: HelpDoc = HelpDoc.Empty
  ): Command[(OptionsType, ArgsType)] = Single(name, helpDoc, options, args)
}
