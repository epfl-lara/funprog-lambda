import stainless.lang._
import stainless.collection._
import stainless.annotation._

object LambdaCalculus {
  sealed abstract class Term 
  case class Var(s: String) extends Term 
  case class App(f: Term, arg: Term) extends Term 
  case class Lam(s: String, body: Term) extends Term 

  def free_vars(t: Term): List[String] = t match {
    case Var(s) => List(s)
    case App(f, arg) => free_vars(f) ++ free_vars(arg)
    case Lam(s, body) => free_vars(body) - s
  }
  def replace(v: String, replW: Term, t: Term): Term =
    t match {
      case Var(s) => if (s==v) replW else t
      case App(f, arg) => App(replace(v,replW,f),
			      replace(v,replW,arg))
      case Lam(s, body) =>
	if (s==v) t else Lam(s, replace(v,replW,body))
    }

  def betaReduce(t: App): Option[Term] = t match {
    case App(Lam(s,body),arg) => Some(replace(s,arg,body))
    case _ => None()
  }
  
  def cbvReduce1(t: Term): Option[Term] = t match {
    case App(Lam(s,body),arg) => Some(replace(s,arg,body))
    case App(f, arg) => cbvReduce1(f) match {
      case Some(fRed) => Some(App(fRed, arg))
      case None() => cbvReduce1(arg) match {
	case Some(argRed) => Some(App(f, argRed))
	case None() => None()
      }
    }
    case _ => None()
  }
  
  def test(x: Int): Boolean = {
    require (0 < x && x < 100)
    (x + x) % 2 == 0
  }.holds
}

