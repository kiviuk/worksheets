import $dep.`org.typelevel::cats-core:2.10.0`
import $dep.`org.typelevel::cats-effect:3.5.2`
import cats._
import cats.data._
import cats.syntax.all._
import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

type Id[A] = A

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}
trait MockUptimeClient extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int]
} 

class LiveUptimeClientp(hosts: Map[String, Int]) extends RealUptimeClient {
  def getUptime(hostname: String): Future[Int] =
    Future.successful(hosts.getOrElse(hostname, 0))
}

class TestUptimeClient(hosts: Map[String, Int]) extends MockUptimeClient {
  def getUptime(hostname: String): Int =
    hosts.getOrElse(hostname, 0)
}

class UptimeService[F[_]:Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    val r: F[List[Int]] = hostnames.traverse(client.getUptime)
    r.map(_.sum) 
}

def testTotalUptime() = {
  val hosts = Map("host1" -> 10, "host2" -> 6)
  val client = new TestUptimeClient(hosts)
  val service = new UptimeService(client)
  val actual = service.getTotalUptime(hosts.keys.toList)
  val expected = hosts.values.sum
  (actual, expected, actual == expected)
}

testTotalUptime()