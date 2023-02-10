import rimp.Interpreter

object Main extends App {
//  val p = new Parser()
  val i = new Interpreter()
  val prog =
    """a := 10;
       a := 9;
       a := 8;
       arr := |1|;
       arr := |2|;
       arr := |3|"""
//  val expected = List(i.AssignThread("t", List(i.If(i.Bop("<",i.Var("a"),i.Var("b")),List(i.Skip),List(i.Assign("a",i.Aop("+",i.Aop("*",i.Var("a"),i.Var("b")),i.Num(1))))))))
  val (env, bStack) = i.eval(i.parse(prog))
  println(env("a"))
  println(env("arr").asInstanceOf[Array[Int]].mkString("Array(", ", ", ")"))
////  println(bStack)
//  val backBlock : i.Block = bStack.map(e => e._2).toList
//  val (env2, bStack2) = i.eval(backBlock, (Map().empty, new mutable.Stack))
//  println(env2("a"))
//  println(env2("arr").asInstanceOf[Array[Int]].mkString("Array(", ", ", ")"))
//  println(bStack2)
//  println(i.backStack)
  val (env2, bStack2) = i.revEval((env, bStack))
  println(env2("a"))
  println(env2("arr").asInstanceOf[Array[Int]].mkString("Array(", ", ", ")"))
}


