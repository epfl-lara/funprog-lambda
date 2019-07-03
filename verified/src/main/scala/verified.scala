import stainless.lang._
import stainless.collection._
import stainless.annotation._

object verified {
  /*
  sealed abstract class Term 
  case class Var(s: String) extends Term 
  case class App(f: Term, arg: Term) extends Term 
  case class Lam(arg: String, body: Term) extends Term 

  def print(t: Term): String = t match {
    case Var(s) => s
    case App(f, arg) => print(f) + "(" + print(arg) + ")"
    case Lam(arg, body) => "(" + arg + "=> " + print(body) + ")"
  }
  */
  
  def test(x: Int): Boolean = {
    require (0 < x && x < 100)
    if (x > 0)
      (6 * x - 1) % 7 == 0
    else 
      (x + x) % 2 == 0
  }.holds

}

