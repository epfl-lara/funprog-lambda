
object RecursiveLanguage {

  sealed abstract class Expr
  case class Const(c: Int) extends Expr
  case class Val(name: String) extends Expr
  case class BinOp(op: BinaryOperator, arg1: Expr, arg2: Expr) extends Expr
  case class IfNonzero(cond: Expr, trueE: Expr, falseE: Expr) extends Expr
  case class Call(function: String, args: List[Expr]) extends Expr
  case class Let(name: String, definition: Expr, rest: Expr) extends Expr

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

  case class Function(params: List[String], body: Expr)
  
  type DefEnv = Map[String, Function]

  val defs : DefEnv = Map[String, Function](
    "fact" -> Function(List("n"),
                       IfNonzero(Val("n"),
                                 BinOp(Times, Val("n"),
                                       Call("fact", List(BinOp(Minus, Val("n"), Const(1))))),
                                 Const(1))
                     )
  )

  def replace(e: Expr, n: String, r: Expr): Expr = e match {
    case Const(c) => e
    case Val(s) => if (s==n) r else e
    case BinOp(op, arg1, arg2) => BinOp(op, replace(arg1, n, r), replace(arg2,n,r))
    case IfNonzero(cond, trueE, falseE) =>
      IfNonzero(replace(cond,n,r), replace(trueE,n,r), replace(falseE,n,r))
    case Call(f, args) => Call(f, args.map(replace(_, n,r)))
    case Let(name, definition, rest) =>
      Let(name, replace(definition,n,r), replace(rest,n,r))
  }
  
  def replaceAll(e: Expr, names: List[String],
                 replacements: List[Expr]): Expr  = (names, replacements) match {
    case (n :: ns, r :: rs) => replaceAll(replace(e, n, r), ns, rs)
    case _ => e                 
  }
  
  def evalExpr(e: Expr): Int = e match {
    case Const(c) => c
    case Val(_) => 0 // should have been replaced
    case BinOp(op, arg1, arg2) =>
      evalBinOp(op)(evalExpr(arg1), evalExpr(arg2))
    case IfNonzero(cond, trueE, falseE) =>
      if (evalExpr(cond) != 0) evalExpr(trueE)
      else evalExpr(falseE)
    case Call(fName, args) => {
      defs.get(fName) match {
        case None => 0
        case Some(f) => {
          val evArgs = args.map((e: Expr) => Const(evalExpr(e)).asInstanceOf[Expr])
          val bodyReplaced = replaceAll(f.body, f.params, evArgs)
          evalExpr(bodyReplaced)
        }
      }
    }
    case Let(name, definition, rest) => {
      val defV = evalExpr(definition)
      evalExpr(replace(rest, name, Const(defV)))
    }
  }

  def main(args: Array[String]): Unit = {
    print("result is: ")
    println(evalExpr(Call("fact", List(Const(5)))))
  }
}
