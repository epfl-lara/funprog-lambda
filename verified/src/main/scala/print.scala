import stainless.lang._
import stainless.collection._
import stainless.annotation._

object print {
  import LambdaCalculus._

  // naive print in Scala-like notation
  def scala(t: Term): String = t match {
    case Var(s) => s
    case App(f, arg) => scala(f) + "(" + scala(arg) + ")"
    case Lam(s, body) => "(" + s + "=> " + scala(body) + ")"
  }
 
  val dot_string: String = ". "
  val lambda_string: String = "\u03BB"
  
  // naive print in lambda notation (with dots)
  def print_long(t: Term): String = {
    t match {
      case Var(s) => s
      case Lam(s,body) =>
	"(" + lambda_string + s + ". " + print_long(body) + ")"
      case App(f,arg) =>
	"(" + print_long(f) + " " + print_long(arg) + ")"
    }
  }

  // nice printing in lambda notation

  def print(t: Term): String = {
    t match {
      case Var(s) => s
      case l@Lam(_,_) => lambda_string + args(l)
      case App(_,_) => apps(t)
    }
  }
  def parenPrint(t: Term): String = {
    t match {
      case Var(s) => s
      case _ => "(" + print(t) + ")"
    }
  }
  def args(t: Lam): String = {
    t match {
    case Lam(arg, body) => arg + (body match {
      case l@Lam(_, _) => " " + args(l)
      case _ => dot_string + print(body)
    })
    }
  }
  def apps(t: Term): String = {
    t match {
      case App(f, e) => apps(f) + " " + parenPrint(e)
      case _ => parenPrint(t)
    }
  }
  
  def strlist(l: List[String]): String =
    List.mkString(l, ", ", (x:String) => x)
}
