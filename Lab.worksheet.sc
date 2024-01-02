import $scalac.`-Yexplain`

val func1 = (x: Int) => x.toDouble
val func2 = (y: Double) => y * 2

val func3a: Int => Double = a => func2(func1(a))
val func3b: Int => Double = func2.compose(func1)

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(func: A => B): F[B]
}

type F[A] = Int => A

def functor = new Functor[F] {
  override def map[A, B](fa: F[A])(func: A => B): F[B] = fa.andThen[B](func)
}

val func: Int => String = (a: Int) => "Mama"

val f: F[String] = functor.map[Int, String](List(1))(func)

f(0) // String = 1


