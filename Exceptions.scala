package exercises

object Exceptions extends App{

  def outOfMemory(i: Int):Unit = throw new OutOfMemoryError;
  def stackError(i: Int): Int = throw new StackOverflowError;

  outOfMemory(1000)
  stackError(1000)

  class PocketCaluclator(val a:Int,val b: Int){

    def add(a:Int,b:Int) = {
      val sum = a+b;
      if(sum > Int.MaxValue)
        throw new OverflowException
      else sum
    }
    def sub(a:Int,b:Int) = {
      val sum = a-b;
      if(sum < Int.MinValue)
        throw new UnderflowException
      else sum
    }
    def multiply(a:Int,b:Int) = a*b
    def divide(a:Int,b:Int) = {
      if(b ==0) throw new MathCalculationException
      else a/b
    }
  }
}

class OverflowException extends Exception

class UnderflowException extends Exception

class MathCalculationException extends Exception
