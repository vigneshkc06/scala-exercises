package exercises

object HOFAndCurries extends App{

  def toCurry(func: Int): ((Int,Int) => Int) => (Int => Int => Int) = ???

  def simple: ((Int,Int) => Int) = ((a,b) =>(a+b))

  println(simple(1,2))

 // def compose: ((Int),(Int)) => Int = (f(x),g(x)) => f(g(x))

  val numbers = List(1,2,3,4)
  val str = List("A","B","C","D")
  println(str.zip(numbers))
  println(str.flatMap(s =>numbers.map(x => s+x)))

}
