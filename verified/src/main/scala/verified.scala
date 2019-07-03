import stainless.lang._
import stainless.collection._
import stainless.annotation._


object verified {
  sealed abstract class Term 
  case class Var(s: String) extends Term 
  case class App(f: Term, arg: Term) extends Term 
  case class Lam(arg: String, body: Term) extends Term 

  object ex {
    val id = Lam("x", Var("x"))
    val first = Lam("x", Lam("y", Var("x")))
    val second = Lam("x", Lam("y", Var("y")))
  }
  
  def printS(t: Term): String = t match {
    case Var(s) => s
    case App(f, arg) => printS(f) + "(" + printS(arg) + ")"
    case Lam(arg, body) => "(" + arg + "=> " + printS(body) + ")"
  }  

  def test(x: Int): Boolean = {
    require (0 < x && x < 100)
    (x + x) % 2 == 0
  }.holds

}

