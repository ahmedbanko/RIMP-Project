
object Main extends App {
  val rimp: RIMP = new RIMP
//  rimp.testEvaluate()
//  print(rimp.lexing_simp(rimp.RIMP_REGS, ("x := 10; arr := [1,2,x]")).filter(_._1 != "whitespace"))
  println(rimp.parse("x := 10; arr := [!x]"))
//  testing git 2
  print(rimp.eval(rimp.parse("x := 10; arr := [!x, 1, (1+2)*0, !x+1]")))
}
