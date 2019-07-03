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
    val twice = Lam("f", Lam("x", App(Var("f"), App(Var("f"), Var("x")))))
    val square = Lam("i", App(App(Var("*"), Var("i")), Var("i")))
    val twice_square = App(App(twice, square), Var("3"))
  }  

  object print {
    def simple(t: Term): String = t match {
      case Var(s) => s
      case App(f, arg) => simple(f) + "(" + simple(arg) + ")"
      case Lam(arg, body) => "(" + arg + "=> " + simple(body) + ")"
    }
    def print(t: Term): String = t match {
      case Var(s) => s
      case l@Lam(_,_) => "\\" + args(l)
      case App(_,_) => apps(t)
    }
    def parenPrint(t: Term): String = t match {
      case Var(s) => s
      case _ => "(" + print(t) + ")"
    }
    def args(t: Lam): String = t match {
      case Lam(arg, body) => arg + (body match {
	  case l@Lam(_, _) => " " + args(l)
	  case _ => "->" + print(body)
      })
    }
    def apps(t: Term): String = t match {
      case App(f, e) => apps(f) + " " + parenPrint(e)
      case _ => parenPrint(t)
    }
  }

  def test(x: Int): Boolean = {
    require (0 < x && x < 100)
    (x + x) % 2 == 0
  }.holds

}

