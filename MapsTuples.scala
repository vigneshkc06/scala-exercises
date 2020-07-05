package exercises

object MapsTuples extends App{

  val map = Map("Jim"-> 555,("JIM",643))
  println(map.map(pair => pair._1.toLowerCase()->pair._2))

}
