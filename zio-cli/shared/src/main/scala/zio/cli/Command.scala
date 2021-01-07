package zio.cli

import zio.cli.HelpDoc.{ h1, Span }
import zio.console.{ getStrLn, putStr, putStrLn, Console }
import zio.{ IO, UIO, ZIO }

import scala.collection.immutable.{ Map => ScalaMap }

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

  def parseInvocation(inv: Invocation, conf: CliConfig): IO[HelpDoc, (Invocation, A)]

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

  def wizard: ZIO[Console, Throwable, Invocation] =
    for {
      flattened <- flatten
      _         <- flattened.render
      cmdId     <- selectCommand(flattened.cmds.keySet)
      sc        = flattened.cmds(cmdId)
      args      <- sc.cmd.wizardInternal
    } yield Invocation(sc.path, args)

  private[cli] def flatten: UIO[Commands] = flattenInternal()

  private[cli] def flattenInternal(ctx: Commands = Commands()): UIO[Commands] =
    self match {
      case c: Command.Single[o, a] =>
        ZIO.succeed(ctx ~ c.name).map(_.record(c))
      case c: Command.Map[a, b] => c.command.flattenInternal(ctx)
      case c: Command.Fallback[a] =>
        for {
          ctx2 <- c.left.flattenInternal(ctx)
          ctx3 <- c.right.flattenInternal(ctx2.next.copy(path = ctx.path))
        } yield ctx3
      case c: Command.Subcommands[a, b] =>
        for {
          ctx2 <- c.parent.flattenInternal(ctx)
          ctx3 <- c.child.flattenInternal(ctx2.next)
        } yield ctx3
    }

  private[cli] def paths: UIO[List[List[String]]] = flatten.flatMap(_.paths)

  private[cli] def selectCommand(cmdIds: Set[Int]): ZIO[Console, Throwable, Int] =
    putStr(s"Please select command: ") *>
      getStrLn
        .flatMap(in => ZIO.effect(in.toInt))
        .orElseFail(new IllegalArgumentException("Not a valid command identifier"))
        .filterOrFail(cmdIds)(
          new IllegalArgumentException(s"Unknown command identifier")
        )
}

object Command {
  final case class BuiltIn(help: Boolean, wizard: Boolean, shellCompletions: Option[ShellType])
  final case class NonRequestedSelection(idx: Int, cmdPath: List[String] = Nil) extends Throwable
  final case class FlatCommand(ord: Int, path: List[String], cmd: Single[_, _]) {
    lazy val mkString: String = String.format("[%2d] %s", ord, path.mkString(" "))
  }

  final case class Commands(
    ord: Int = 1,
    path: Vector[String] = Vector.empty,
    cmds: ScalaMap[Int, FlatCommand] = ScalaMap.empty
  ) {
    def next: Commands           = Commands(ord + 1, path, cmds)
    def ~(seg: String): Commands = Commands(ord, path :+ seg, cmds)
    def record(s: Single[_, _]): Commands =
      Commands(ord, path, cmds + (ord -> FlatCommand(ord, path.toList, s)))
    def current: FlatCommand = cmds(ord)
    def render: ZIO[Console, Nothing, Unit] =
      ZIO.foreach(cmds.values.toList.sortBy(_.ord))(fc => putStrLn(fc.mkString)).unit

    def paths: UIO[List[List[String]]] =
      ZIO
        .succeedNow(cmds.values.toList)
        .map(_.sortBy(_.ord))
        .map(_.map(_.path))
  }

  final case class Invocation(cmdPath: List[String], optsArgs: List[String]) {
    lazy val mkString: String = (cmdPath ::: optsArgs).mkString(" ")
  }

  object Invocation {
    val empty: Invocation = Invocation(Nil, Nil)
  }

  final case class InvocationContext(current: Vector[String] = Vector.empty, paths: List[List[String]] = Nil) {
    def ~(seg: String): InvocationContext          = InvocationContext(current :+ seg, paths)
    def record: InvocationContext                  = InvocationContext(current, current.toList :: paths)
    def path(p: Vector[String]): InvocationContext = InvocationContext(p, paths)
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

    override def parseInvocation(
      inv: Invocation,
      conf: CliConfig
    ): IO[HelpDoc, (Invocation, (OptionsType, ArgsType))] = {
      def validate(cTail: List[String], args: List[String]) =
        for {
          tuple               <- self.options.validate(args, conf)
          (args, optionsType) = tuple
          tuple               <- self.args.validate(args, conf)
          (args, argsType)    = tuple
          _ <- ZIO.when(cTail.isEmpty && args.nonEmpty)(
                ZIO.fail(HelpDoc.p(Span.error(s"Unexpected arguments for command $name: $args")))
              )
        } yield (Invocation(cTail, args), (optionsType, argsType))

      inv.cmdPath match {
        case cHead :: cTail if cHead == name => validate(cTail, inv.optsArgs)
        case Nil                             => validate(Nil, Nil)
        case _                               => ZIO.fail(HelpDoc.p(Span.error(s"Bad invocation for command $name: ${inv.mkString}")))
      }
    }

    def synopsis: UsageSynopsis =
      UsageSynopsis.Named(name, None) + options.synopsis + args.synopsis

    private[cli] def wizardInternal: ZIO[Console, Throwable, List[String]] =
      putStrLn("=" * 100) *>
        putStrLn(s"Selected command: $name") *>
        Wizard.show(synopsis) *>
        options.wizard.zipWith(args.wizard)(_ ++ _)
  }

  final case class Map[A, B](command: Command[A], f: A => B) extends Command[B] {
    def helpDoc = command.helpDoc

    final def parse(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, (List[String], B)] = command.parse(args, conf).map {
      case (leftover, a) => (leftover, f(a))
    }

    override def parseInvocation(inv: Invocation, conf: CliConfig): IO[HelpDoc, (Invocation, B)] =
      command.parseInvocation(inv, conf).map {
        case (leftover, a) => (leftover, f(a))
      }

    def synopsis: UsageSynopsis = command.synopsis
  }

  final case class Fallback[A](left: Command[A], right: Command[A]) extends Command[A] {
    def helpDoc = left.helpDoc + right.helpDoc

    final def parse(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, (List[String], A)] = left.parse(args, conf) orElse right.parse(args, conf)

    override def parseInvocation(inv: Invocation, conf: CliConfig): IO[HelpDoc, (Invocation, A)] =
      left.parseInvocation(inv, conf) orElse right.parseInvocation(inv, conf)

    def synopsis: UsageSynopsis = UsageSynopsis.Mixed
  }

  final case class Subcommands[A, B](parent: Command[A], child: Command[B]) extends Command[(A, B)] {
    def helpDoc = parent.helpDoc + h1("subcommands") + child.helpDoc

    final def parse(
      args: List[String],
      conf: CliConfig
    ): IO[HelpDoc, (List[String], (A, B))] = parent.parse(args, conf).flatMap {
      case (leftover, a) => child.parse(leftover, conf).map(t => (t._1, (a, t._2)))
    }

    override def parseInvocation(inv: Invocation, conf: CliConfig): IO[HelpDoc, (Invocation, (A, B))] =
      for {
        parentResult        <- parent.parseInvocation(inv, conf)
        (parentLeftover, a) = parentResult
        childResult         <- child.parseInvocation(parentLeftover, conf)
        (childLeftover, b)  = childResult
      } yield (childLeftover, (a, b))

    def synopsis: UsageSynopsis = parent.synopsis
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
