import $dep.`org.typelevel::cats-core:2.10.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.data._
import cats.syntax.all._
import cats.implicits._

def listTraverse[F[_]: Applicative, A, B](
    list: List[A]
)(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
    (accum, func(item)).mapN(_ :+ _)
  }

def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
  listTraverse(list)(identity)

listSequence(List(Vector(1, 2), Vector(3, 4)))

listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))