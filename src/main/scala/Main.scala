import rimp.{RIMP_tester, Tokenizer}

object Main extends App {
  val rimp: RIMP_tester = new RIMP_tester
//  rimp.testEvaluate()
//  print(rimp.lexing_simp(rimp.RIMP_REGS, ("x := 10; arr := [1,2,x]")).filter(_._1 != "whitespace"))
//  println(rimp.parse("arr := [1, 2, 3]; arr1[2] := 10"))
//  print(rimp.eval(rimp.parse("arr := [1, 2, 3]; i1before := arr[1]; arr[1] := 10; i1after := arr[1]")))
val t = new Tokenizer()
print(t.tokenize("arr := [1, 2, 3]; i1before := arr[1]; arr[1] := 10; i1after := arr[1]"))



}


//val t = new Thread {
//  override def run(): Unit = {
//    println("Running in a new thread")
//  }
//}
//t.start()