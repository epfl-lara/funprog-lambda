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
  val omega = App(self,self) 
  val twice = Lam("f", Lam("x", App(Var("f"), App(Var("f"), Var("x")))))
  val doubling = Lam("i", App(App(Var(plusSym), Var("i")), Var("i")))
  val twice_doubling = App(App(twice, doubling), Var("3"))
  val abs = Lam("x",
		App(App(App(Var(ifSym),
                  App(App(Var("<"), Var("x")), Var("0"))),
		  App(App(Var(minusSym), Var("0")), Var("x"))),
		  Var("x")))
  val abs42 = App(abs, Var("42"))
  val firstAB = App(App(first, Var("a")), Var("b"))
  val captureFirst = App(App(first, Var("y")), Var("z"))

  val terms = List(
    "id" -> id,
    "self id" -> selfid,
    "twice_doubling" -> twice_doubling,
    "abs42" -> abs42,
    "firstAB" -> firstAB,
    "captureFirst" -> captureFirst
  )

  def showTrace(t: Term): String =
    List.mkString(cbvTrace(t, 50), "\n ==> ", print.print)
  
  def showNamedTerm(p: (String,Term)): String =
    p._1 + ":  " + showTrace(p._2)

  val toShow: String =
    List.mkString(terms, "\n", showNamedTerm) 
}
