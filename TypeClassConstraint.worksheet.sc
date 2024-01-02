// Third-party type class A
trait A[A]

// Your type class MyTC
trait MyTC[B] {
  def myMethod(value: B): String
}

// Type class constraint: MyTC requires evidence that A is available for type B
trait RequiresA[B]

object RequiresA {
  implicit val forListString: RequiresA[List[String]] = new RequiresA[List[String]] {
    def getAInstance: A[List[String]] = new A[List[String]] {} // Instantiate A here
  }

  implicit val forOptionInt: RequiresA[Option[Int]] = new RequiresA[Option[Int]] {
    def getAInstance: A[Option[Int]] = new A[Option[Int]] {} // Instantiate A here
  }
}

// Instances of MyTC for types that satisfy RequiresA
given [B](using ev: RequiresA[B]): MyTC[B] with {
  def myMethod(value: B): String = s"MyTC instance for ${value.toString}"
}

// Example usage
val myListString: List[String] = List("hello", "world")
val myOptionInt: Option[Int] = Some(42)
val myVectorDouble: Vector[Double] = Vector(3.14, 2.71)

val result1 = summon[MyTC[List[String]]].myMethod(myListString)
val result2 = summon[MyTC[Option[Int]]].myMethod(myOptionInt)
// Compilation error if uncommented:
// summon[MyTC[Vector[Double]]].myMethod(myVectorDouble)
// RequiresA instance for Vector[Double] is not provided

println(result1) // MyTC instance for List(hello, world)
println(result2) // MyTC instance for Some(42)