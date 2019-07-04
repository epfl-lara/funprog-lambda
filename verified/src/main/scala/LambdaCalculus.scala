import stainless.lang._
import stainless.collection._
import stainless.annotation._

object LambdaCalculus {
  sealed abstract class Term 
  case class Var(s: String) extends Term 
  case class App(f: Term, arg: Term) extends Term 
  case class Lam(s: String, body: Term) extends Term 

  // Variables used in t that are not bound by lambda
  def free_vars(t: Term): List[String] = t match {
    case Var(s) => List(s)
    case App(f, arg) => free_vars(f) ++ free_vars(arg)
    case Lam(s, body) => free_vars(body) - s
  }

  // Naively replace v with replW in t
  def replace(v: String, replW: Term, t: Term): Term =
    t match {
      case Var(s) => if (s==v) replW else t
      case App(f, arg) => App(replace(v,replW,f),
			      replace(v,replW,arg))
      case Lam(s, body) =>
	if (s==v) t // nothing to replace
	else Lam(s, replace(v,replW,body))
    }

  // String containing k apostrofy symbols
  def strOf(k: Long): String =
    if (k <= 0) "" else strOf(k-1) + "'"

  // Return a name similar to s but distinct from names in avoid
  def getFresh(s: String, k: Long, avoid: List[String]): String = {
    val s1 = s + strOf(k)
    if (avoid.contains(s1)) getFresh(s, k+1, avoid)
    else s1
  }

  // Capture-avoiding substitution
  def replaceSafe(v: String, replW: Term, t: Term): Term =
    t match {
      case Var(s) => if (s==v) replW else t
      case App(f, arg) => App(replaceSafe(v,replW,f),
			      replaceSafe(v,replW,arg))
      case Lam(s, body) => 
	if (s==v) t else {
	  val fv = free_vars(replW)
	  if (fv.contains(s)) { // the only differce with 'replace'
	    val s1 = getFresh(s, 1, fv)
	    val body1 = replace(s, Var(s1), body) // rename s -> s1
	    Lam(s1, replace(v, replW, body1)) // replace in renamed
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

  // Tries to reduce at top level if possible
  def betaReduce(t: Term): Option[Term] = t match {
    case App(Lam(s,body),arg) =>    // beta reduction
      Some(replaceSafe(s,arg,body))
    case App(App(App(Var(ifSym), // if is lazy
          Var(cond)), trueBranch), falseBranch) =>
      if (cond==trueSym) Some(trueBranch)
      else if (cond==falseSym) Some(falseBranch)
      else None()
    case App(App(Var(s), Var(n1)), Var(n2)) => binOpStr(s,n1,n2)    
    case _ => None()
  }

  // Recursively tries to find the first redex in following 'App'
  // If found, reduce it once and return
  def cbvReduce1(t: Term): Option[Term] =
    betaReduce(t) match {
      case Some(tr) => Some(tr) // redex was on top
      case None() => t match {
	case App(f, arg) => cbvReduce1(f) match {// reduce f first
	  case Some(fRed) => Some(App(fRed, arg)) // enough for now
	  case None() => cbvReduce1(arg) match { // reduce arg
	    case Some(argRed) => Some(App(f, argRed))
	    case None() => None() // nothing could be reduced
	  }
	}
	case _ => None() // do not reduce underneath lambda
      }
    }

  // Applies cbvReduce1 at most max times and gives list of steps
  def cbvTrace(t: Term, max: Int): List[Term] = {
    require(0 <= max)
    if (max==0) List(t)
    else cbvReduce1(t) match {
      case None() => List(t)
      case Some(tRed) => t :: cbvTrace(tRed, max-1)
    }
  }
}
