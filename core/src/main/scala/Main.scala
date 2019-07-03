object Main {
  def main(args: Array[String]): Unit = {
    import verified._
    println(s"Running test(3) ==> ${test(3)}")
    println(s"Lambda first ==> ${print.print(ex.twice_square)}")
    println("Here is another println.")
  } 
}
