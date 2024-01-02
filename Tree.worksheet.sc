import $scalac.`-Yexplain`
import $dep.`org.typelevel::cats-core:2.9.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.syntax.all._

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  given treeFunctor: Functor[Tree] with {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
  extension [A](fa: Tree[A]) {
    def map[B](f: A => B): Tree[B] = treeFunctor.map(fa)(f)
  }
}

import Tree.{treeFunctor, map}

Branch[Int](Branch[Int](Leaf(1), Leaf(2)), Leaf(3)).map(_ + 1)