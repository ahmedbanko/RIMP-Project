import rimp.Interpreter

object Main extends App {
//  val p = new Parser()
  val i = new Interpreter()
//  val threadProg =
//    """a := 1000;
//       xt2 := 0;
//      thread t1 := {
//        write "t1 start\n";
//        while (!a > 1) do {
//          skip;
//          a := !a - 1
//        };
//         write "t1 end\n"
//      };
//      thread t2 := {
//        xt2 := 1996;
//        write "t2 start\n";
//        skip;
//        write "t2 end\n"
//       };
//        run ?t1;
//        run ?t2;
//        write !a;
//        write !xt2""".stripMargin
//  val expected = List(i.AssignThread("t", List(i.If(i.Bop("<",i.Var("a"),i.Var("b")),List(i.Skip),List(i.Assign("a",i.Aop("+",i.Aop("*",i.Var("a"),i.Var("b")),i.Num(1))))))))
////  println(i.parse(threadProg))
////  println(i.parse(threadProg) == expected)

  val prog = "arr := [1+1, 2, 3]; i1before := arr[1]; arr[1] := 10; i1after := arr[1]"
print(i.eval(i.parse(prog)))
}


