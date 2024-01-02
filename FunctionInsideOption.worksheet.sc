trait Monad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    this.flatMap(value)(a => pure[B](func(a)))
}

case class Taxation[A](id: A)

val taxationMonad = new Monad[Taxation] {
  def pure[A](a: A): Taxation[A] = Taxation(a)

  def flatMap[A, B](value: Taxation[A])(func: A => Taxation[B]): Taxation[B] =
    func(value.id)
}

extension [A](taxation: Taxation[A]) {
  def map[B](func: A => B): Taxation[B] = taxationMonad.map[A,B](taxation)(func)
  def flatMap[B](func: A => Taxation[B]): Taxation[B] = taxationMonad.flatMap[A,B](taxation)(func)
  def pure[B](b: B): Taxation[B] = taxationMonad.pure[B](b)
}

val taxation: Taxation[Double] = Taxation[Double](9)

for {
    t <- taxation
    s <- Taxation[String](t.toString())
} yield s // Taxation[String] = Taxation(9.0)


////////////////////// TEST
val taxation2: Taxation[String] =
  taxationMonad.flatMap[Double, String](taxation)(id => Taxation[String](id.toString()))