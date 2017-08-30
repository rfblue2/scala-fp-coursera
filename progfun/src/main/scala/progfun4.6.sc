trait Expr {
  def eval(): Int = this match {
    case Number(n) => n
    case Var(x) => throw new Error("Var has no value")
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }

  def show(): String = this match {
    case Number(n) => n.toString
    case Var(x) => x
    case Sum(e1, e2) => e1.show + " + " + e2.show
    case Prod(Sum(s1, s2), e2) => "(" + s1.show + " + " +
      s2.show + ") * " + e2.show
    case Prod(e1, Sum(s1, s2)) => e1.show + " * (" +
      s1.show + " + " + s2.show + ")"
    case Prod(Sum(s1, s2), Sum(s3, s4)) => "(" + s1.show +
      " + " + s2.show + ") * (" + s3.show + " + " +
      s4.show + ")"
    case Prod(e1, e2) => e1.show + " * " + e2.show
  }
}

case class Number(n: Int) extends Expr

case class Var(x: String) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

val num1 = Number(1)
val num2 = Number(2)
val num3 = Number(3)

val sum = Sum(num1, num2)
val prod = Prod(num1, num2)

Prod(sum, num3).show
Sum(prod, num3).show

num1.show
num2.show
sum.show
sum.eval
Prod(Sum(Number(3), Var("x")), Var("y")).show