import stainless.lang._
import stainless.collection._
import stainless.annotation._

object LambdaCalculus {
  sealed abstract class Term 
  case class Var(s: String) extends Term 
  case class App(f: Term, arg: Term) extends Term 
  case class Lam(arg: String, body: Term) extends Term 

  object ex {
    val id = Lam("y", Var("y"))
    val first = Lam("x", Lam("y", Var("x")))
    val second = Lam("x", Lam("y", Var("y")))
    val self = Lam("x", App(Var("x"), Var("x")))
    val selfid = App(self, id)
    val twice = Lam("f", Lam("x", App(Var("f"), App(Var("f"), Var("x")))))
    val square = Lam("i", App(App(Var("times"), Var("i")), Var("i")))
    val twice_square = App(App(twice, square), Var("three"))
  }  

  object print {
    val dot_string: String = ". "
    val lambda_string: String = "\u03BB"
    def scala(t: Term): String = t match {
      case Var(s) => s
      case App(f, arg) => scala(f) + "(" + scala(arg) + ")"
      case Lam(arg, body) => "(" + arg + "=> " + scala(body) + ")"
    }
    def print(t: Term): String = t match {
      case Var(s) => s
      case l@Lam(_,_) => lambda_string + args(l)
      case App(_,_) => apps(t)
    }
    def parenPrint(t: Term): String = t match {
      case Var(s) => s
      case _ => "(" + print(t) + ")"
    }
    def args(t: Lam): String = t match {
      case Lam(arg, body) => arg + (body match {
	  case l@Lam(_, _) => " " + args(l)
	  case _ => dot_string + print(body)
      })
    }
    def apps(t: Term): String = t match {
      case App(f, e) => apps(f) + " " + parenPrint(e)
      case _ => parenPrint(t)
    }

    def strlist(l: List[String]): String =
      List.mkString(l, ", ", (x:String) => x)
  }

  def free_vars(t: Term): List[String] = t match {
    case Var(s) => List(s)
    case App(f, arg) => free_vars(f) ++ free_vars(arg)
    case Lam(arg, body) => free_vars(body) - arg
  }

  def test(x: Int): Boolean = {
    require (0 < x && x < 100)
    (x + x) % 2 == 0
  }.holds
}
