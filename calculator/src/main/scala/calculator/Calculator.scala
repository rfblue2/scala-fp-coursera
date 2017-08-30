package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.foldLeft(Map[String, Signal[Double]]())((map, pair) => {
      map.updated(pair._1, Signal(eval(pair._2.apply(), namedExpressions)))
    })
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def internalEval(iExpr: Expr, stackVars: List[String]): Double = {
      iExpr match {
        case Literal(v) => v
        case Ref(name) =>
          if (stackVars contains name) Double.NaN
          else internalEval(getReferenceExpr(name, references), name :: stackVars)
        case Plus(a, b) => internalEval(a, stackVars) + internalEval(b, stackVars)
        case Minus(a, b) => internalEval(a, stackVars) - internalEval(b, stackVars)
        case Times(a, b) => internalEval(a, stackVars) * internalEval(b, stackVars)
        case Divide(a, b) => internalEval(a, stackVars) / internalEval(b, stackVars)
      }
    }
    internalEval(expr, List())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
