import stainless.lang._
import stainless.collection._
import stainless.annotation._

object ExpressionLanguage {
  sealed abstract class Expr
  case class Const(c: BigInt) extends Expr
  case class BinOp(op: BinaryOperator, arg1: Expr, arg2: Expr) extends Expr
  case class IfNonzero(cond: Expr, trueE: Expr, falseE: Expr) extends Expr

  sealed abstract class BinaryOperator
  case object Plus extends BinaryOperator
  case object Minus extends BinaryOperator
  case object Times extends BinaryOperator
  case object Power extends BinaryOperator
  case object LessEq extends BinaryOperator

  def evalBinOp(op: BinaryOperator)(x: BigInt, y: BigInt): BigInt = op match {
    case Plus => x + y
    case Minus => x - y
    case Times => x * y
    case Power => x.pow(y.toInt)
    case LessEq => if (x <= y) 1 else 0
  }

  def eval(e: Expr): BigInt = e match {
    case Const(c) => c
    case BinOp(op, arg1, arg2) =>
      evalBinOp(op)(eval(arg1), eval(arg2))
    case IfNonzero(cond, trueE, falseE) =>
      if (eval(cond) != 0) eval(trueE)
      else eval(falseE)
  }

  def expr1 = BinOp(Times, Const(6), Const(7))       // 6*7
  def cond1 = BinOp(LessEq, expr1, Const(50))        // expr1 < 50
  def expr2 = IfNonzero(cond1, Const(10), Const(20)) // if (cond1) 10 else 20
  def expr3 = BinOp(Power, Const(10), Const(100))    // 10^100

  def strOp(op: BinaryOperator): String = op match {
    case Plus => "+"
    case Minus => "-"
    case Times => "*"
    case Power => "^"
    case LessEq => "<="
  }

  def str(e: Expr): String = e match {
    case Const(c) => c.toString
    case BinOp(op, arg1, arg2) =>
      "(" + str(arg1) + " "  + strOp(op) + " " + str(arg2) + ")"
    case IfNonzero(cond, trueE, falseE) =>
      "(if (" + str(cond) + ") " + 
      str(trueE) + " else " + str(falseE) + ")"
  }

  def show(e: Expr): Unit = {
    println(str(e))
    println(" ~~> " + eval(e))
    println
  }
  def main(args: Array[String]): Unit = {
    show(expr1)
    show(expr2)
    show(expr3)
  }
}
