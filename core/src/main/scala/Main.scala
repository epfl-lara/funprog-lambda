object Main {
  def main(args: Array[String]): Unit = {
    import LambdaCalculus._
    println(s"Running test(3) ==> ${test(3)}")
    println(s"Lambda first ==> ${print.print(ex.twice_square)}")
    println(s"Free vars ==> ${print.strlist(free_vars(ex.twice_square))}")
    println(s"Self-app ==> ${print.print(ex.selfid)}")
    println("Here is another println.")
  } 
}
