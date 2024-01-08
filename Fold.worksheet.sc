import $dep.`org.typelevel::cats-core:2.10.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.data._
import cats.syntax.all._
import cats.implicits._


val l = List(4,3,2,1)
val e = List.empty[Int]

l.foldLeft(List.empty[Int])( (xs,x) => x :: xs )
l.foldRight(List.empty[Int])( (x,xs) => x :: xs )

def listMap[A,B](xs: List[A], f: A => B): List[B] = {
  xs.foldRight(List.empty[B])( (x,xs) => f(x) :: xs)
}

listMap(l, _ + 1)

def flatMap[A, B](xs: List[A], f: A => List[B]): List[B] = {
  listMap(xs,f).flatten
}

flatMap[Int, String](l, x => List("<" + x.toString() + ">"))
flatMap[Int, String](e, x => List("<" + x.toString() + ">"))

def listFilter[A](xs: List[A], f: A => Boolean): List[A] = {
  xs.foldRight(List.empty[A])( (x,xs) => if f(x) then x :: xs else List.empty[A] )
}

listFilter[Int](l, _ > 2)

def listSum[A](xs: List[A], f: (A, A) => A, zero: A): A = {
  xs.foldRight(zero)( (acc, a) => f(acc, a))
}

listSum[Int](l, _ + _, 0)

import cats.Eval
import cats.Foldable
def bigData = (1 to 1_000_000_000).to(LazyList)
// bigData.foldRight(0L)(_ + _)

// https://scastie.scala-lang.org/kiviuk/Fkn4BddrQoasbIwtXDVPBA/11
// https://scastie.scala-lang.org/kiviuk/KwRRd3rhTjWxaMO5LhQfvQ/1
import cats.instances.lazyList._ // for Foldable
val eval: Eval[Long] =
  Foldable[LazyList].
    foldRight(bigData, Eval.now(0L)) { (num, eval) =>
      eval.map(_ + num)
}

// eval.value

val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
(Foldable[List] compose Foldable[Vector]).combineAll(ints)