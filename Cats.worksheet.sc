import cats.syntax.monoid
import $scalac.`-Yexplain`
import $dep.`org.typelevel::cats-core:2.9.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.implicits._

val c = 1
val d = 1

Monoid[Int].combine(c, d)

case class Order(totalCost: Double, quantity: Double)

trait AddTrait[A] {
  def add(ls: List[A]): A
  def optionAdd(ls: List[Option[A]]): A
}

object IntAdd extends AddTrait[Int]:
  def add(ls: List[Int]): Int =
    ls match {
      case h :: xs => h |+| add(xs)
      case _       => Monoid[Int].empty
    }
  def optionAdd(ls: List[Option[Int]]): Int = add(ls.flatMap(identity))

IntAdd.add((1 to 100).toList)
val intOptions: List[Option[Int]] = (1 to 100).toList.map(Option(_))
IntAdd.optionAdd(intOptions)

val orders = (1 to 2).toList.map(x => Order(x, x + 1))

orders.map(o => (o._1 * o._2))
orders.foldLeft(Monoid[Double].empty)((acc, num) => (acc |+| num._1 * num._2))

def addAll[A](values: List[A])(using monoid: Monoid[A]): A =
  values.foldRight(monoid.empty)(_ |+| _)

addAll(List(None, Some(1), Some(2)))

val func =
  ((x: Int) => x.toDouble).
    map(x => x + 1).
    map(x => x * 2).
    map(x => s"${x}!")
func(123)

