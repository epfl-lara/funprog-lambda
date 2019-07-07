import stainless.lang._
import stainless.collection._
import stainless.annotation._
// import stainless.io.StdOut.println

object EnvBased {
  import LambdaCalculus._
  
  sealed abstract class Value
  case class LongVal(l: Long) extends Value
  case class FunVal(f: Value => Value) extends Value
  case class Data(t: Term) extends Value

  case class Env(f: String => Option[Value]) {
    def apply(x: String) = f(x)
    def update(s: String, v: => Value): Env = {
      def f1(s1: String): Option[Value] = {
	if (s1==s) Some(v) else f(s1)
      }
      Env(f1)
    }
  }
   
  def liftLongBinOp(opName: String, op: (Long, Long) => Long): Value = {
    FunVal((v1:Value) =>
      FunVal((v2: Value) =>
	(v1, v2) match {
	  case (LongVal(i1), LongVal(i2)) => LongVal(op(i1,i2))
	  case _ => Data(Var(opName + "? ?"))
	}
      )
    )
  }

  def lessThan(x: Long, y: Long): Long =
    if (x < y) 1L else 0L
  
  def initEnvFun(s: String): Option[Value] = {
    if (s=="+") Some(liftLongBinOp("+", _ + _))
    else if (s=="-") Some(liftLongBinOp("-", _ - _))
    else if (s=="<") Some(liftLongBinOp("-", lessThan))
    else None()
  }
  val initEnv = Env(initEnvFun)
  
  def evalS(env: Env, t: Term): Value = {
    // println("Evaluating " + print.print(t))
    t match {
      case Var(s) => str2long(s) match {
	case Some(i) => LongVal(i) // number
	case None() => env(s) match {
	  case Some(v) => v // defined variable
	  case None() => {
	    // println("could not find " + s + " in env")
	    Data(t) // undefined
	  }
	}
      }
      case Lam(n, body) => {
	def f(x: Value) = evalS(env.update(n, x), body)
	FunVal(f)
      }
      case App(Var(s), Lam(recCallName, body)) if s==recSym => {
	def evalBody: Value = evalS(env.update(recCallName, evalBody), body)
	evalBody
      }
      case App(App(App(Var(s), cond), trueBranch), falseBranch) if s==ifSym =>
	evalS(env, cond) match {
	  case LongVal(i) => {
	    if (i==1) evalS(env,trueBranch)
	    else if (i==0) evalS(env,falseBranch)
	    else Data(App(App(App(Var(ifSym), Var(long2str(i))), trueBranch), falseBranch))
	  }
	  case _ => Data(t)
	}
      case App(fT, argT) => evalS(env, fT) match {
	case FunVal(f) => f(evalS(env, argT))
	case _ => Data(t) // stuck
      }
    }
  }
}
