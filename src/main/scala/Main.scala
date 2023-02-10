import rimp.Interpreter

object Main extends App {
  val i = new Interpreter()
  val exampleProg1 =
    """n := 1;
       n := 3;
       n := 0"""
//       while (!n > 0) do {
//          fact := !n * !fact;
//          n := !n - 1;
//          c := !c + 1
//       }
//       """
  val parsed = i.parse(exampleProg1)
//  parsed.foreach(println)
//  println("----------------")
//  parsed.reverse.foreach(println)
  val env = i.eval(parsed)
  println(env)
  println("----------------")
//  println(i.revStmtEval(i.stmtStack.toList, env))
//  i.envStack.foreach(println)
//  println(i.stmtStack)

}


