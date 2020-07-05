package exercises

import scala.language.postfixOps

object MethodNotation extends  App{

  class Person(name: String, favouriteMovie: String,val age: Int){

    def +(name: String) : Person = new Person(s"$this.name ($name)",favouriteMovie,age)
    def unary_+ : Person =  new Person(name,favouriteMovie,age+1)
    def learns(lang: String) :String = s"$name learns $lang"
    def learnsScala: String = learns("Scala")
    def apply(times: Int) = s"$name watched $favouriteMovie $times times"
  }

  val john = new Person("John","Apocalypto",30)
  println(john + "the slayer")
  val john1 = +john
  println(john1.age)
  println(john learns "Scala")
  println(john(10))

}
