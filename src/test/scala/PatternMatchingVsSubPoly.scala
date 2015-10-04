import org.scalatest.FunSuite

class PatternMatchingVsSubPoly extends FunSuite {

  test("pattern matching example") {

    sealed trait Expr
    case class Add(e1: Expr, e2: Expr) extends Expr
    case class Sub(e1: Expr, e2: Expr) extends Expr
    case class Num(n: Int) extends Expr

    def value(e: Expr): Int = e match {
      case Add(e1, e2) => value(e1) + value(e2)
      case Sub(e1, e2) => value(e1) - value(e2)
      case Num(n) => n
    }
    val expr = Add(Num(5), Sub(Num(50), Num(10)))

    val result = value(expr)

    println(result)

  }

  test("sub poly example") {
    sealed trait Expr {
      def value: Int
    }

    case class Add(e1: Expr, e2: Expr) extends Expr {
      def value = e1.value + e2.value
    }

    case class Sub(e1: Expr, e2: Expr) extends Expr {
      def value = e1.value - e2.value
    }

    case class Num(n: Int) extends Expr {
      def value = n
    }
    val expr = Add(Num(5), Sub(Num(50), Num(10)))

    val result = expr.value

    println(result)
  }

}
