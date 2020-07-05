package exercises

import scala.util.Random

object Optional extends App{

  val config :Map[String,String] = Map(
    "host" -> "176.45.36.1",
    "port" -> "80"
  )

  class Connection {
    def connect = "Connected"
  }

  object Connection {
    val random = new Random(System.nanoTime())
    def apply(host: String,port: String): Option[Connection] =
      if (random.nextBoolean()) Some(new Connection)
      else None
  }

  val conn = config.get("host").flatMap(h => config.get("port").flatMap(p => Connection(h,p)))
  val cv = conn.map(c => c.connect)
  cv.foreach(println)

  val cv1 = for {
    h <- config.get("host")
    p <- config.get("port")
    conn <- Connection(h,p)
  }yield conn.connect

  cv1.foreach(println)
}
