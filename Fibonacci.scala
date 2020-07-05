
package playground

import scala.io.StdIn.readInt


object Fibonacci {


    def fibonacci(x:Int):Int = {

      if(x < 2) 0
      else {
        def fibonacciInner(x: Int, last: Int, nextLast: Int): Int = {
          if (x < 3) last
          else fibonacciInner(x - 1, last + nextLast, last)
        }
        fibonacciInner(x,1,0)
      }
    }



    def main(args: Array[String]) {
      /** This will handle the input and output**/
      println(fibonacci(readInt()))

    }


}
