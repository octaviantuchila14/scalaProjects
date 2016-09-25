package calculator

import calculator.Divide

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
    //val res = Map.empty[String, Signal[Expr]]
    val res = namedExpressions.map(namedEx => {
      namedEx._1 -> Signal(eval(namedEx._2(), namedExpressions.filterKeys(_ != namedEx._1)))
    });
    res
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    println("Expression: " + expr.toString)
    println("References: " + references.toString)
    //references.values.foreach(x => println(x()))

    expr match {
      case Literal(v) => v
      case Ref(name) => eval(getReferenceExpr(name, references), references.filterKeys(_ != name))
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
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
