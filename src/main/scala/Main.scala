
object Main extends App {
  val rimp: RIMP = new RIMP
//  rimp.testEvaluate()
//  print(rimp.lexing_simp(rimp.RIMP_REGS, ("arr[10, 10]")).filter(_._1 != "whitespace"))
  println(rimp.parse("[1, 2, 3]"))
//  testing git 2
}
