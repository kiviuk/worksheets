import $scalac.`-Yexplain`

trait Printable[A] {
  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] =
    val orig = this
    new Printable[B] {
      def format(value: B): String = {
        orig.format(func(value))
      }
    }
}
final case class Box[A](boxedValue: A)

def decodeBoxTo[A]: Box[A] => A = box => box.boxedValue

given b: Printable[Boolean] with {
  override def format(value: Boolean): String = if (value) s"true" else s"false"
}

/*
 implicit val printableBoolean: Printable[Boolean] = new Printable[Boolean] {
  override def format(value: Boolean): String = if (value) "true" else "false"
} */

given s: Printable[String] with {
  override def format(value: String): String = s"value = '$value'"
}

b.format(true) // true
s.format("hello") // 'hello'

val printableBooleanBox: Printable[Box[Boolean]] =
  b.contramap[Box[Boolean]](decodeBoxTo[Boolean])

printableBooleanBox.format(Box(false))

val printableStringBox: Printable[Box[String]] =
  s.contramap[Box[String]](decodeBoxTo[String])

printableStringBox.format(Box("Bitches")) // 'Bitches'
