import rimp.Interpreter

object Main extends App {
//  val p = new Parser()
  val i = new Interpreter()
  val threadProg =
    """a := 1000;
      thread t1 := {
        while (!a > 0) do {
          skip;
          a := !a - 1
        }
      };
      thread t2 := {
        skip
       };
      run ?t1;
      run ?t2""".stripMargin
  val expected = List(i.AssignThread("t", List(i.If(i.Bop("<",i.Var("a"),i.Var("b")),List(i.Skip),List(i.Assign("a",i.Aop("+",i.Aop("*",i.Var("a"),i.Var("b")),i.Num(1))))))))
//  println(i.parse(threadProg))
//  println(i.parse(threadProg) == expected)
  println(i.eval(i.parse(threadProg)))
}


