import java.util.concurrent.TimeUnit
import scala.util.Failure
import scala.util.Success
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import cats.effect.kernel.Resource
import scala.concurrent._
import $scalac.`-Yexplain`
import $dep.`org.typelevel::cats-core:2.9.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.data._
import cats.syntax.all._

case class User(age: Int)

// Option Functor
// https://youtu.be/QGWlgaiM5_c?t=194
// val oo: Option[User] = Option(User(1))

// val loo: List[Option[User]] = List(oo)

// val oloo = OptionT(loo)

// val re2: OptionT[List, Int] =
//   for {
//     a <- oloo
//   } yield a.age

// val re3 = re2.map(x => x + 1)

// type ErrorOr[A] = Either[String, A]

// type ErrorOrOption[A] = OptionT[ErrorOr, A]

// val e = 10.pure[ErrorOrOption]

/////////////////////////////////////////////////////////
type Response[A] = EitherT[Future, String, A]

val globalExecutionContext: ExecutionContext =
  scala.concurrent.ExecutionContext.global

import cats.instances.future._ // for Monad
import scala.concurrent.ExecutionContext.Implicits.global

def getPowerLevel(autobot: String): Response[Int] =
  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  powerLevels.get(autobot) match
    case Some(pl) => EitherT.right(Future(pl))
    case None     => EitherT.left(Future(s"$autobot?"))

def canSpecialMove(a1: String, a2: String): Response[Boolean] = for {
  power1 <- getPowerLevel(a1)
  power2 <- getPowerLevel(a2)
} yield (power1 + power2) > 15

def tacticalReport(a1: String, a2: String): String =
  val s = canSpecialMove(a1, a2).value

  Await.result(s, Duration(1, TimeUnit.MILLISECONDS)) match
    case Left(value) => value
    case Right(value) => value.toString

tacticalReport("Jazz", "Hot Rod") 