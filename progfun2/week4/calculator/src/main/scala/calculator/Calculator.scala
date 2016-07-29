package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {


    val mapValues = namedExpressions map { case (k, v) => (k, Signal(eval(Map.empty, k, v(), namedExpressions))) }
    mapValues

  }

  def eval(dependencyMap: Map[String, List[String]], ref: String, expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Plus(a, b) => eval(dependencyMap, ref, a, references) + eval(dependencyMap, ref, b, references)
      case Minus(a, b) => eval(dependencyMap, ref, a, references) - eval(dependencyMap, ref, b, references)
      case Times(a, b) => eval(dependencyMap, ref, a, references) * eval(dependencyMap, ref, b, references)
      case Divide(a, b) => eval(dependencyMap, ref, a, references) / eval(dependencyMap, ref, b, references)
      case Ref(name) => {
        name match {
          case x if x == ref => eval(dependencyMap, ref, Literal(Double.NaN), references)
          case _ => eval(dependencyMap, ref, getReferenceExpr(name, references), references)
        }


      }
      case _ => throw new Error("Unknown expression")
    }
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
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
