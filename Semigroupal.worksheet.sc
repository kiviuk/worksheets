import $dep.`org.typelevel::cats-core:2.10.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.data._
import cats.syntax.all._
import cats.implicits._

Semigroupal[List].product(List(1, 2), List(3, 4))

List(1,2).flatMap(x => List(3,4).map(y => (x,y)))

val res = List(6,-10,3,7).zipWithIndex.map{
  case (stop, pos) => (math.abs(10 - stop), pos)
}.sortWith(  (pairA, pairB) => pairA < pairB  ).map{
  case (diff, pos) => pos
}.take(1)

val closestIndex = List(6,-10,8,7).zipWithIndex.minBy {
  case (value, _) => math.abs(10 - value)
}._2

val closestIndex2 = List(6, -10, 8, 7).zipWithIndex.foldLeft((Int.MaxValue, -1)) {
  case ((minDiff, minIndex), (value, index)) =>
    val currentDiff = math.abs(10 - value)
    if (currentDiff < minDiff) (currentDiff, index) else (minDiff, minIndex)
}._2

println(closestIndex)


val numbers = List(1, 2, 3, 4, 5)

val result = numbers.traverse { x =>
  Option(x * 2)
}