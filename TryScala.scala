package exercises

import scala.util.{Random, Try}

object TryScala extends App {

  val hostName = "localhost"
  val port = "8080"

  def renderHtml(page: String) = println(page)

  class Connection {
    def getUrl(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) "<html>...<\\html>"
      else throw new RuntimeException("Connection Interupted")
    }
  }

  object HttpService {
    val random = new Random(System.nanoTime())

    def getConnection(h: String, p: String): Connection =
      if (random.nextBoolean()) new Connection
      else throw new RuntimeException("Port Interupted")
  }

  Try(HttpService.getConnection(hostName, port))
    .flatMap(c => Try(c.getUrl(hostName + port)).map(renderHtml(_)))

  for {
    c <- Try(HttpService.getConnection(hostName, port))
    p <- Try(c.getUrl(hostName + port))
  } yield renderHtml(p)

}
