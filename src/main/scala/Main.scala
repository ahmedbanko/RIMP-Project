
object Main extends App {
  val rimp: RIMP = new RIMP
//  rimp.testEvaluate()
//  print(rimp.lexing_simp(rimp.RIMP_REGS, ("arr := [10]")).filter(_._1 != "whitespace"))
  println(rimp.parse("arr := [10]"))
//  testing git
}
