
object Main extends App {
  val rimp: RIMP = new RIMP
//  rimp.testEvaluate()
//  print(rimp.lexing_simp(rimp.RIMP_REGS, ("x := 10; arr := [1,2,x]")).filter(_._1 != "whitespace"))
//  println(rimp.parse("arr1 := [1, 2, 3]; i1 := arr1[0]"))
//  testing git 2
  print(rimp.eval(rimp.parse("arr1 := [1, 2, 3]; i1 := arr1[2]")))
}
