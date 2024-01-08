import $dep.`org.typelevel::cats-core:2.10.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.data._
import cats.syntax.all._
import cats.implicits._


val numbers = List(1, 2, 3, 4, 5)

// Use foldMap to transform and + combine elements
val product = numbers.foldMap(identity)

println(product) // Output: 120



// Define a function that transforms an integer into a string
val intToString: Int => String = _.toString

// Use foldMap to apply the function to each element in the list and combine the results
val result: String = Foldable[List].foldMap(numbers)(intToString)

println(result) // Prints: "12345"

val product2 = numbers.foldMap(intToString)


final case class Cat(
    name: String,
    yearOfBirth: Int,
    favoriteFoods: List[String]
  )
  val tupleToCat: (String, Int, List[String]) => Cat =
    Cat.apply _
  val catToTuple: Cat => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)
  implicit val catMonoid: Monoid[Cat] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

  val garfield = Cat("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))
  garfield |+| heathcliff

  List(1).flatMap(x => List(2).flatMap(y => List(x,y)))

  for {
    a <- List(1)
    b <- List(2)
    c <- List(a,b)
  } yield c
