import $dep.`org.typelevel::cats-core:2.9.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.data._
import cats.syntax.all._

object TaglessFinal {
  trait Algebra[E[_]] {
    def b(bool: Boolean): E[Boolean]
    def i(int: Int): E[Int]
    def or(left: E[Boolean], right: E[Boolean]): E[Boolean]
    def and(left: E[Boolean], right: E[Boolean]): E[Boolean]
    def sum(left: E[Int], right: E[Int]): E[Int]
  }

  case class SimpleExp[A](value: A)

  given simpleAlg: Algebra[SimpleExp] with {
    override def b(bool: Boolean): SimpleExp[Boolean] = SimpleExp(bool)
    override def i(int: Int): SimpleExp[Int] = SimpleExp(int)
    override def or(
        left: SimpleExp[Boolean],
        right: SimpleExp[Boolean]
    ): SimpleExp[Boolean] = SimpleExp(left.value || right.value)
    override def and(
        left: SimpleExp[Boolean],
        right: SimpleExp[Boolean]
    ): SimpleExp[Boolean] = SimpleExp(left.value && right.value)
    override def sum(
        left: SimpleExp[Int],
        right: SimpleExp[Int]
    ): SimpleExp[Int] = SimpleExp(left.value + right.value)
  }

  given optionAlg: Algebra[Option] with {
    override def b(bool: Boolean): Option[Boolean] = Option(bool)
    override def i(int: Int): Option[Int] = Option(int)
    override def or(
        left: Option[Boolean],
        right: Option[Boolean]
    ): Option[Boolean] = left.flatMap(l => right.map(r => l || r))
    override def and(
        left: Option[Boolean],
        right: Option[Boolean]
    ): Option[Boolean] = left.flatMap(l => right.map(r => l && r))
    override def sum(left: Option[Int], right: Option[Int]): Option[Int] =
      left.flatMap(l => right.map(r => l + r))
  }

}

object TaglessFinalTest {
  trait Console[F[_]] {
    def putStrLn(line: String): F[Unit] 
    val getStrLn: F[String]
  }
  // Manual implementation of Monad for ConsoleTest
  implicit val consoleTestMonad: Monad[ConsoleTest] = new Monad[ConsoleTest] {
    override def pure[A](a: A): ConsoleTest[A] = ConsoleTest(data => (data, a))

    override def flatMap[A, B](fa: ConsoleTest[A])(f: A => ConsoleTest[B]): ConsoleTest[B] =
      ConsoleTest(data => {
        val (newData, a) = fa.run(data)
        f(a).run(newData)
      })
    override def tailRecM[A, B](a: A)(f: A => ConsoleTest[Either[A, B]]): ConsoleTest[B] = {
      def loop(current: ConsoleData, a: A): (ConsoleData, B) = {
        f(a).run(current) match {
          case (newData, Left(nextA))  => loop(newData, nextA)
          case (newData, Right(result)) => (newData, result)
        }
      }

      ConsoleTest(data => loop(data, a))
    }
  }

  final case class ConsoleData(input: List[String], output: List[String])

  final case class ConsoleTest[A](run: ConsoleData => (ConsoleData, A))
  
  object ConsoleTest {
    implicit def consoleConsoleTest: Console[ConsoleTest] =
      new Console[ConsoleTest] {
        def putStrLn(line: String): ConsoleTest[Unit] =
          ConsoleTest(data => (data.copy(output = line :: data.output), ()))

        val getStrLn: ConsoleTest[String] =
          ConsoleTest(data =>
            (data.copy(input = data.input.drop(1)), data.input.head)
          )
      }
  }

  def consoleProgram[F[_]: Console: Monad]: F[String] = {
    val console = implicitly[Console[F]]

    import console._

    for {
      _     <- putStrLn("What is your name?")
      name  <- getStrLn
      _     <- putStrLn(s"Hello, $name, good to meet you!")
    } yield name
  }

}

import TaglessFinal.optionAlg._

val summm = sum(Some(1), None)

println(summm)

import TaglessFinalTest._



val res = TaglessFinalTest.consoleProgram[ConsoleTest].run(ConsoleData(List("John"), List.empty))

res._1