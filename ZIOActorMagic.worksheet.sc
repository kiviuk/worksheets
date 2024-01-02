import $dep.`org.typelevel::cats-core:2.9.0`
import $dep.`dev.zio::zio:2.0.20`

import zio._

trait Message[+A]

object Exp1:
  trait AskActor[Response, -Message[+_]] {
    def send(message: Message[Any]): ZIO[Any, Nothing, Unit]
    def ask(message: Message[Response]): ZIO[Any, Nothing, Response]
  }

  case object StringMessage extends Message[String]
  object StringAskActor extends AskActor[String, Message] {
    override def ask(message: Message[String]): ZIO[Any, Nothing, String] = ???
    override def send(message: Message[Any]): ZIO[Any, Nothing, Unit] = ???
  }
end Exp1

object Exp2:
  trait AskActor[-Message[+_]] {
    def send(message: Message[Any]): ZIO[Any, Nothing, Unit]
    def ask[Response](message: Message[Response]): ZIO[Any, Nothing, Response]
  }

  trait SendActor[-In] {
    def send(message: In): ZIO[Any, Nothing, Unit]
  }
end Exp2

object Exp3:
  // https://www.47deg.com/blog/scala-3-enumerations/
  sealed trait TempMessage[+Response]
  final case class SetTemp(temp: Double) extends TempMessage[Unit]
  case object GetTemp extends TempMessage[Double]
  
end Exp3

object Actors extends ZIOAppDefault {
  val run =
    ZIO.debug("Hello, world!")
}
