import stainless.lang._
import stainless.collection._
import stainless.annotation._

object ExpressionLanguage {

  sealed abstract class Expr
  case class Const(c: BigInt) extends Expr
  case class Val(name: String) extends Expr
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

  def evalExpr(e: Expr): BigInt = e match {
    case Const(c) => c
    case Val(_) => 0
    case BinOp(op, arg1, arg2) =>
      evalBinOp(op)(evalExpr(arg1), evalExpr(arg2))
    case IfNonzero(cond, trueE, falseE) =>
      if (evalExpr(cond) != 0) evalExpr(trueE)
      else evalExpr(falseE)
  }

  def expr1 = BinOp(Times, Const(6), Const(7))
  def cond1 = BinOp(LessEq, expr1, Const(50))
  def expr2 = IfNonzero(cond1, Const(10), Const(20))
  def expr3 = BinOp(Power, Const(10), Const(100))

  def main(args: Array[String]): Unit = {
    print("expr1 --> ")
    println(evalExpr(expr1))
    print("expr2 --> ")
    println(evalExpr(expr2))
    print("expr3 --> ")
    println(evalExpr(expr3))
  }

}
