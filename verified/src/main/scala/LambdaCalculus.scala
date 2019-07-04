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

  @extern
  def strOf(k: Long): String =
    if (k <= 0) "" else strOf(k-1) + "'"

  def getFresh(s: String, k: Long, avoid: List[String]): String = {
    val s1 = s + strOf(k)
    if (avoid.contains(s1)) getFresh(s, k+1, avoid)
    else s1
  }
  
  def replaceSafe(v: String, replW: Term, t: Term): Term =
    t match {
      case Var(s) => if (s==v) replW else t
      case App(f, arg) => App(replaceSafe(v,replW,f),
			      replaceSafe(v,replW,arg))
      case Lam(s, body) => 
	if (s==v) t else {
	  val fv = free_vars(replW)
	  if (fv.contains(s)) {
	    val s1 = getFresh(s, 1, fv)
	    val body1 = replace(s, Var(s1), body)
	    Lam(s1, replace(v, replW, body1))
	  } else Lam(s, replace(v, replW, body))
	}
    }

  val plusSym = "+"
  val minusSym = "-"
  val ltSym = "<"
  val trueSym = "1"
  val falseSym = "0"
  val ifSym = "if"
  
  def binOp(op: String, x: Long, y: Long): Option[Long] = {
    if (op==plusSym) Some(x + y)
    else if (op==minusSym) Some(x - y)
    else if (op==ltSym) Some(if (x < y) 1 else 0)
    else None()
  }
  
  @extern
  def binOpStr(op: String, x: String, y: String): Option[Term] = {
    try {
      binOp(op, x.toLong, y.toLong) match {
	case Some(i) => Some(Var(i.toString))
	case None() => None()
      }
    } catch {
      case _: java.lang.NumberFormatException => None()
    }
  }

  def betaReduce(t: Term): Option[Term] = t match {
    case App(Lam(s,body),arg) =>
      Some(replaceSafe(s,arg,body))
    case App(App(App(Var(ifSym),
          Var(cond)), trueBranch), falseBranch) =>
      if (cond==trueSym) Some(trueBranch)
      else if (cond==falseSym) Some(falseBranch)
      else None()
    case App(App(Var(s), Var(n1)), Var(n2)) => binOpStr(s,n1,n2)    
    case _ => None()
  }
  
  def cbvReduce1(t: Term): Option[Term] =
    betaReduce(t) match {
      case Some(tr) => Some(tr)
      case None() => t match {
	case App(f, arg) => cbvReduce1(f) match {
	  case Some(fRed) => Some(App(fRed, arg))
	  case None() => cbvReduce1(arg) match {
	    case Some(argRed) => Some(App(f, argRed))
	    case None() => None()
	  }
	}
	case _ => None()
      }
    }

  def cbvTrace(t: Term, max: Int): List[Term] = {
    require(0 <= max)
    if (max==0) List(t)
    else cbvReduce1(t) match {
      case None() => List(t)
      case Some(tRed) => t :: cbvTrace(tRed, max-1)
    }
  }
}
