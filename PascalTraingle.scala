package playground

import scala.io.StdIn.readInt;

object PascalTraingle {


  def printPascalTraingle(i: Int, acc: String): String = {
    def getPascalTriangleInner(n: Int, r: Int, acc: String): String = {
      if (r > n) acc
      else {
        val pascalValue : BigInt = getFactorial(n) / (getFactorial(r) * getFactorial(n - r))
        getPascalTriangleInner(n, r + 1, pascalValue + " " + acc);
      }
    }
    if (i < 0) acc
    else
      printPascalTraingle(i - 1, getPascalTriangleInner(i, 0, "") + "\n" + acc)
  }


  def getFactorial(i: Int): BigInt = {
    def getFactorialInternal(i: Int, acc: BigInt): BigInt = {
      if (i < 2) acc
      else getFactorialInternal(i - 1, acc * i)
    }

    getFactorialInternal(i, 1)
  }

  def main(args: Array[String]) {
    val n = readInt()
    val str = printPascalTraingle(n - 1, "")
    println(str)
  }

}
