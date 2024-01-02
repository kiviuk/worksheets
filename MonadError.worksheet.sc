import $scalac.`-Yexplain`
import $dep.`org.typelevel::cats-core:2.9.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import scala.util.Try
import java.lang.IllegalArgumentException
import cats._
import cats.syntax.all._

def validateAdult[F[_]](age: Int)(using  me: MonadError[F, Throwable ]): F[Int] =

  val success: F[Int] = me.pure(age)
  val failure = IllegalArgumentException("Age must be 18 or older.")
  me.ensure(success)(failure)(age => !(age < 18))

val s: Try[Int] = validateAdult[Try](18)
val f: Try[Int] = validateAdult[Try](0)