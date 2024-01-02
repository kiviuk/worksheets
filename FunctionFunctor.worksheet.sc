trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// Functor Type Class for a Function Type C => X

// The purpose of this Functor is to allow applying a function (A => B)
// to the result of another function (C => A), producing a new function (C => B).

// In a typical scenario, 'fa' might be a List[A] or Option[A],
// allowing 'f: A => B' to operate on elements of type A within the List or Option.
// However, a Functor doesn't care how these elements of type A are obtained.
// They could also come from another function 'fa: C => A'.

// This means 'f: A => B' can be applied to 'fa: C => A', creating a new function 'C => B'.
// Essentially, it's function composition, applying 'f' to the result of 'fa', yielding type B.

// For example, 'functionFunctor[String]' is equivalent to a Functor for 'F[String => X]'.
// It allows you to compose functions, transforming a function from C to A into a function from C to B.
given [C]: Functor[[X] =>> C => X] with {
  def map[A, B](fa: C => A)(f: A => B): C => B = x => f(fa(x))
}

// Example usage
val fa_add2: String => Int = a => a.length() + 2

// A = Int, B = Int, C = String
val result1: String => Int =
  given_Functor_Function[String].map[Int, Int](fa_add2)(a => a - 4)

// A = Int, B = String
val result2: String => String =
  given_Functor_Function[String].map[Int, String](fa_add2)(a => s"${a - 4}")

result1("3") // Int = -1
result2("3") // String = -1

// C = String
val functorInstance = summon[Functor[[X] =>> String => X]]

// B = Int, A = Int (due to a.length has type Int)
// C => A == String => Int
// => X == Int
val mappedFunction: String => Int =
  functorInstance.map[String, Int]((c: String) => c)((a: String) => a.length)

mappedFunction("hello") // Int = 5

// Existing type class A from the 3rd party library
trait thirdPartyTypeClass[A] {
  def specialFlatMap(value: A): A
}

// New type class MyTC
trait MyTC[B] {
  def myMethod(value: B): String
}

// Instances of A
given AInt: thirdPartyTypeClass[Int] with {
  def specialFlatMap(value: Int): Int = value * 2
}

given AString: thirdPartyTypeClass[String] with {
  def specialFlatMap(value: String): String = value.toUpperCase()
}

// Extension method for types with A[Int]
extension (value: Int)(using ev: thirdPartyTypeClass[Int]) {
  def specialFlatMap = ev.specialFlatMap(value)
}

// Extension method for types with A[String]
extension (value: String)(using ev: thirdPartyTypeClass[String]) {
  def specialFlatMap = ev.specialFlatMap(value)
}

// Instances of MyTC for types that have an instance of A
given MyTCForInt(using AInt: thirdPartyTypeClass[Int]): MyTC[Int] with {
  def myMethod(value: Int): String = s"MyTCForInt: ${value.specialFlatMap}"
}

// In this part, using AString: A[String] provides evidence of A[String],
// and the given instance MyTCForString requires a MyTC[String].
// The implicit conversion comes into play here because MyTCForString is
// expecting a MyTC[String], but we have an instance of A[String].
// The given instance we defined earlier is responsible for implicitly
// converting the A[String] instance to a MyTC[String] instance.
given MyTCForString(using AString: thirdPartyTypeClass[String]): MyTC[String] with {
  def myMethod(value: String): String = s"MyTCForString: ${value.specialFlatMap}"
}

// Example usage
val resultInt = summon[MyTC[Int]].myMethod(42)
val resultString = summon[MyTC[String]].myMethod("hello")

println(resultInt)    // MyTCForInt: 84
println(resultString) // MyTCForString: HELLO

