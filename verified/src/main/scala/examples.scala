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

  // testing variable capture
  val captureFirst = App(App(first, Var("y")), Var("z"))

  // evaluation order: argument not used vs used twice
  val manyOps = App(App(Var("+"), App(App(Var("+"), Var("3")), Var("4"))),
		    Var("9"))
  val evalOrder0 = App(Lam("x", Var("5")), manyOps)
  val evalOrder2 = App(Lam("x", App(App(Var("+"), Var("x")), Var("x"))),
		       manyOps)

  // booleans
  val trueT = first
  val falseT = second
  val orTerm = Lam("p", Lam("q", App(App(Var("p"), trueT), Var("q"))))
  val orTest = App(App(orTerm, falseT), trueT)

  // pair
  def pair(t1: Term, t2: Term) = Lam("f", App(App(Var("f"), t1), t2))
  def fst(t: Term) = App(t, first)
  def snd(t: Term) = App(t, second)
  val triple1 = pair(Var("a"), pair(Var("b"), Var("c")))
  val getSecond = fst(snd(triple1))
  
  // lists
  val nilT = Lam("m", Lam("n", Var("m")))
  def cons(h:Term, t:Term) = Lam("m",Lam("n", App(App(Var("n"), h), t)))
  def mkVarList(l: List[String]): Term = l match {
    case Nil() => nilT
    case Cons(h,t) => cons(Var(h),mkVarList(t))
  }
  def headT(t: Term): Term = App(App(t, Var("?")),
				 Lam("h",Lam("t", Var("h"))))
  def tailT(t: Term): Term = App(App(t, Var("?")),
				 Lam("h",Lam("t", Var("t"))))
  val getlist2nd = headT(tailT(mkVarList(List("1","2","3"))))

  def Y(F: Term): Term = {
    val fLoop = Lam("x", App(F, App(Var("x"), Var("x"))))
    App(fLoop, fLoop)
  }

  def op2(op: String, arg1: Term, arg2: Term) =
    App(App(Var(op), arg1), arg2)
  def mkIf(cond: Term, trueB: Term, falseB: Term) = {
    App(App(App(Var("if"), cond), trueB), falseB)
  }
  def mkRecursive(body: Term): Term = {
    val fLoop = Lam("f", replace("self", App(Var("f"), Var("f")), body))
    App(fLoop, fLoop)
  }
  def mul = mkRecursive(Lam("x", Lam("y",
    mkIf(op2("<", Var("y"), Var("1")),
         Var("0"),
	 op2("+",
	   Var("x"),
  	   op2("self", Var("x"), op2("-", Var("y"), Var("1"))))))))

  val mul52 = App(App(mul, Var("5")), Var("2"))
			
  val terms = List(
    "id" -> id,
    "self id" -> selfid,
    "twice_doubling" -> twice_doubling,
    "abs42" -> abs42,
    "firstAB" -> firstAB,
    "captureFirst" -> captureFirst,
    "evalOrder0" -> evalOrder0,
    "evalOrder2" -> evalOrder2,
    "getSecond" -> getSecond,
    "getlist2nd" -> getlist2nd,
    "Y(second)" -> Y(second),
    "mul52" -> mul52
  )
  val termStrict = List(
    "evalOrder0" -> evalOrder0,
    "evalOrder2" -> evalOrder2,
    "getlist2nd" -> getlist2nd,
    "mul52" -> mul52
  )

  val termsEval = List(
    "3+4" -> App(App(Var("+"), Var("3")), Var("4")),
    "abs42" -> abs42,
    "manyOps" -> manyOps,
    "evalOrder0" -> evalOrder0,
    "evalOrder2" -> evalOrder2
  )
  
  def showTrace(t: Term, nonStrict: Boolean): String =
    List.mkString(trace(t, nonStrict, 50), "\n ==> ", print.print)
  
  def showNamedTerm(nonStrict: Boolean)(p: (String,Term)): String = {
    p._1 +
    (if (nonStrict) " (non-strict): " else " (strict): ") +
    showTrace(p._2, nonStrict)
  }
  def showEval(p: (String,Term)): String = {
    import EnvBased._
    val msg = p._1
    val t = p._2
    val tS = evalS(initEnv, t) match {
      case FunVal(f) => "<function>"
      case LongVal(l) => long2str(l)
      case Data(t) => print.print(t)
    }
    msg + "(eval): " + tS
  }

  val toShow: String =
    List.mkString(termsEval, "\n", showEval) + "\n"  +
    List.mkString(terms, "\n", showNamedTerm(true)) + "\n" +
    List.mkString(termStrict, "\n", showNamedTerm(false)) + "\n"    
}
