import stainless.lang._
import stainless.collection._
import stainless.annotation._

object ExpressionLanguage {

  sealed abstract class Expr
  case class IntConst(c: Int) extends Expr
  case class BinOp(op: BinaryOperator, arg1: Expr, arg2: Expr) extends Expr
  case class IfNonzero(cond: Expr, trueE: Expr, falseE: Expr) extends Expr

  sealed abstract class BinaryOperator
  case object Plus extends BinaryOperator
  case object Minus extends BinaryOperator
  case object Times extends BinaryOperator
  case object LessEq extends BinaryOperator

  def evalBinOp(op: BinaryOperator)(x: Int, y: Int): Int = op match {
    case Plus => x + y
    case Minus => x - y
    case Times => x * y
    case LessEq => if (x <= y) 1 else 0
  }

  def evalExpr(e: Expr): Int = e match {
    case IntConst(c) => c
    case BinOp(op, arg1, arg2) =>
      evalBinOp(op)(evalExpr(arg1), evalExpr(arg2))
    case IfNonzero(cond, trueE, falseE) =>
      if (evalExpr(cond) != 0) evalExpr(trueE)
      else evalExpr(falseE)
  }
}
