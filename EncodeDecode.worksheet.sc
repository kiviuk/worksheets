// Codec[A] knows how to handle things of type A
// A is the mediator type for decode and encode
trait Codec[A] {
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] =
    val orig = this
    new Codec[B] {

      // encode: HighLevel(B) to LowLevel(String)
      //         Scala => Javascript => String
      // first we receive Scala through def encode, then we turn Scala to
      // Javascript via enc: B => A,
      // then pass the Javascript to the Codec for Javascript to turn
      // Javascript in String via orig.encode(a)
      def encode(value: B): String =
        val a: A = enc(value)
        orig.encode(a)
        // assume type A = javascript
        // assume type B = Scala
        // encode takes Scala code, runs the Scala code through enc(value)
        // to receive javascript (A), then runs the javascript through
        // encode(a) to obtain the target encoding from Scala to String
        // Javascript being the intermediary language platform
        // Scala => Javascript => String

      // decode: LowLevel(String) => HighLevel(B)
      //         String => Javascript => Scala
      // First we receive a String through def decode, then we turn
      // the String into Javascript via orig.decode(value) and
      // then turn Javascript into Scala via dec: A => B
      def decode(value: String): B =
        val a: A = orig.decode(value)
        dec(a)
        // assume type A = javascript
        // assume type B = Scala
        // decode takes a String runs it through orig.decode(value)
        // to receive javascript (A)
        // then runs A through dec
        // to obtain the target language B (Scala)
        // Javascript being the intermediary language platform
        // String => Javascript => Scala
    }
}

final case class Scala[A](value: A)

given Codec[Int] with {
  def encode(value: Int): String = s"$value"
  def decode(value: String): Int = value.toInt
}

given Codec[Double] with {
  def encode(value: Double): String = s"$value"
  def decode(value: String): Double = value.toDouble
}

given Codec[String] with {
  def encode(value: String): String = s"$value"
  def decode(value: String): String = value
}

def scalaCodec[T](using code: Codec[T]) = new Codec[Scala[T]] {
  def encode(value: Scala[T]): String = s"${value.value}"
  def decode(value: String): Scala[T] = Scala(code.decode(value))
}

val stringToScalaInt: Scala[Int] = scalaCodec[Int].decode("1")
val scalaInt2String: String = scalaCodec[Int].encode(Scala(10))

val stringToScalaDouble: Scala[Double] = scalaCodec[Double].decode("1.0")
val scalaDouble2String: String = scalaCodec[Double].encode(Scala(10.0))

// (dec: A => B, enc: B => A)
// def imap[B](dec: A => B, enc: B => A): Codec[B] =
// if T is of type Double in
// def scalaCodec[T](using code: Codec[T]) = new Codec[Scala[T]]
// => then A is Scala[Double] in trait Codec[A]
// A is the source type, B is the target type
// => B is a free parameter, B determines dec and enc functions

def doubleToInt(sd: Scala[Double]) = Scala(sd.value.toInt)
def intToDouble(si: Scala[Int]) = Scala(si.value.toDouble)

def stringToDouble(ss: Scala[String]): Scala[Double] = Scala(ss.value.toDouble)
def doubleToString(sd: Scala[Double]): Scala[String] = Scala(sd.value.toString)

val scalaDoubleCodec: Codec[Scala[Double]] = scalaCodec[Double]
val scalaIntFromDoubleCodec: Codec[Scala[Int]] =
  scalaDoubleCodec.imap[Scala[Int]](doubleToInt, intToDouble)

scalaIntFromDoubleCodec.decode("0") // Scala[Int] = Scala(0)
scalaIntFromDoubleCodec.encode(Scala(3)) // String = 0.0

val scalaIntCodec: Codec[Scala[Int]] = scalaCodec[Int]
val scalaDoubleFromIntCodec: Codec[Scala[Double]] =
  scalaIntCodec.imap[Scala[Double]](intToDouble, doubleToInt)

scalaDoubleFromIntCodec.decode("3") // Scala[Double] = Scala(0.0)
scalaDoubleFromIntCodec.encode(Scala(3.14)) // String = 3 (loosing precision!)

val scalaDoubleCodec2: Codec[Scala[Double]] = scalaCodec[Double]
val scalaStringFromDoubleCode: Codec[Scala[String]] =
  scalaDoubleCodec2.imap[Scala[String]](doubleToString, stringToDouble)
scalaStringFromDoubleCode.decode("3.14")  // Scala[String] = Scala(3.14)
scalaStringFromDoubleCode.encode(Scala("3.14")) // String = 3.14
