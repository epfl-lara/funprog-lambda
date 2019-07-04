import stainless.lang._
import stainless.collection._
import stainless.annotation._

object ex {
  import LambdaCalculus._

  val id = Lam("y", Var("y"))
  val first = Lam("x", Lam("y", Var("x")))
  val second = Lam("x", Lam("y", Var("y")))
  val self = Lam("x", App(Var("x"), Var("x")))
  val selfid = App(App(self, id), Var("a"))
  val twice = Lam("f", Lam("x", App(Var("f"), App(Var("f"), Var("x")))))
  val square = Lam("i", App(App(Var("times"), Var("i")), Var("i")))
  val twice_square = App(App(twice, square), Var("three"))

  val terms = List(
    "id" -> id,
    "self id" -> selfid,
    "twice_square" -> twice_square    
  )

  def showNamedTerm(p: (String,Term)): String =
    p._1 + ":  " + print.print(p._2)
  def showTrace(t: Term): String =
    List.mkString(cbvTrace(t, 20), "\n==>", print.print)

  val toShow: String =
    List.mkString(terms, "\n", showNamedTerm) + "\n" +
    showTrace(twice_square) + "\n" +
    showTrace(selfid)
  
}
