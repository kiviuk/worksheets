import $scalac.`-Yexplain`
import $dep.`org.typelevel::cats-core:2.9.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.syntax.all._

case class Person[A](age: A)

object Person {
  given personFunctor: Functor[Person] with {
    def map[A, B](fa: Person[A])(f: A => B): Person[B] = Person(f(fa.age))
  }

  extension [A](fa: Person[A]) {
    def map[B](f: A => B): Person[B] = personFunctor.map(fa)(f)
  }
}

import Person.{personFunctor, map}

def incId(p: Person[Int]): Person[Int] = map(p)(_ + 1)

val p4: Person[Int] = Person(4)
val p5 = incId(p4)
val p42 = p4.map[String](_.toString() + 2)
