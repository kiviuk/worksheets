trait Functor[F[_]] {
  def map[A, B](fa: F[A])(func: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

given Applicative[List] with {
  def pure[A](a: A): List[A] = List(a)
  def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
    for {
      a <- fa
      f <- ff
    } yield f(a)

  def map[A, B](fa: List[A])(func: A => B): List[B] =
    fa.map(func)
}

// Example usage
val addTwo: Int => Int => Int = a => b => a + b

val optionInt: List[Int] = List(1,2)
val optionFunction: List[Int => Int => Int] = List(addTwo)

val result: List[Int => Int] = summon[Applicative[List]].ap(optionFunction)(optionInt)

val apply1: Int => Int = result(0)
val apply2: Int => Int = result(1)


// List(1,2) <*> addTwo => List(addTwo(1), addTwo(2))
// addTwo(1)(1) = 2
// addTwo(1)(2) = 3
// addTwo(1)(3) = 4
// ...
// addTwo(1)(10) = 11
for x <- 1 to 10 yield apply1(x)

// addTwo(2)(1) = 3
// addTwo(2)(2) = 4
// addTwo(2)(3) = 5
// ...
// addTwo(2)(10) = 12
for x <- 1 to 10 yield apply2(x)