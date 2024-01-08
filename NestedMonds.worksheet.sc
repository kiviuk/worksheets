import $dep.`org.typelevel::cats-core:2.10.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.data._
import cats.syntax.all._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// https://www.geekabyte.io/2018/05/thoughts-on-dealing-with-having-another.html
// https://scastie.scala-lang.org/kiviuk/Fkn4BddrQoasbIwtXDVPBA/8

val a: Future[Either[String, Int]] = Future.successful(Right(1))

def func1(x: Int): Option[Int] = Some(10 + x)
def func2(x: Int): Int = 10 + x

val result: Future[Int] = EitherT(a).foldF(
  error => Future.successful(0),
  value => OptionT.fromOption[Future](func1(value)).getOrElseF(Future.successful(0))
)

val result2: Future[Int] = EitherT(a).foldF(
  error => Future.successful(0),
  value => OptionT.fromOption[Future](func1(value))
    .fold(func2(value))(identity)
)

val result3: Future[Int] = EitherT(a).foldF(
  error => Future.successful(0),
  value => OptionT.fromOption[Future](func1(value))
    .flatMap(v => OptionT.some[Future](func2(v)))
    .getOrElseF(Future.successful(0))
)

val result4: Future[Int] = for {
  value <- EitherT(a).foldF(
    error => Future.successful(0),
    value => OptionT.fromOption[Future](func1(value)).getOrElseF(Future.successful(0))
  )
  finalResult <- OptionT.some[Future](func2(value)).getOrElseF(Future.successful(0))
} yield finalResult