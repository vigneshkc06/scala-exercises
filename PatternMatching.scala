package exercises

object PatternMatching extends App{

  trait Expr

  case class Number(var n: Int) extends Expr

  case class Sum(e1: Expr,e2: Expr) extends Expr

  case class Product(e1: Expr,e2: Expr) extends Expr

  def show(e: Expr): String ={
    e match {
      case Number(n) => s"$n"
      case Sum(e1,e2) => s"${show(e1)}+${show(e2)}"
      case Product(e1,e2) => {
        def showP(e:Expr): String = {
          e match {
            case Number(_) => show(e)
            case Product(_,_) => show(e)
            case _ => s"(${show(e)})"
          }
        }
        showP(e1)+"*"+showP(e2)
      }
    }
  }

  println(show(Number(1)))
  println(show(Sum(Number(1),Number(2))))
  println(show(Product(Sum(Number(1),Number(2)),Number(3))))

}
