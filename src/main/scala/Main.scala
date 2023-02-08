import rimp.Interpreter

object Main extends App {
//  val p = new Parser()
  val i = new Interpreter()
  val threadProg =
    """thread t1 := {
        write "t1 start\n";
        while (!a > 1) do {
          skip
        };
         write "t1 end\n"
      };
      thread t2 := {
        write "t2 start\n";
        a := 1;
        write "t2 end\n"
       };
        run ?t1;
        run ?t2"""
//  val expected = List(i.AssignThread("t", List(i.If(i.Bop("<",i.Var("a"),i.Var("b")),List(i.Skip),List(i.Assign("a",i.Aop("+",i.Aop("*",i.Var("a"),i.Var("b")),i.Num(1))))))))
  println(i.eval(i.parse(threadProg)))
//  println(i.parse(threadProg) == expected)

//  val prog = "arr := [1]"
//print(i.eval(i.parse(prog)))
}


