import rimp.Interpreter

import scala.collection.mutable

object Main extends App {
//  val p = new Parser()
  val i = new Interpreter()
  val prog =
    """a := 10;
       a := 9;
       a := 8"""
//  val expected = List(i.AssignThread("t", List(i.If(i.Bop("<",i.Var("a"),i.Var("b")),List(i.Skip),List(i.Assign("a",i.Aop("+",i.Aop("*",i.Var("a"),i.Var("b")),i.Num(1))))))))
  val (env, bStack) = i.eval(i.parse(prog))
  print(env)
  println(bStack)
  val backBlock : i.Block = bStack.map(e => e._2).toList
  val env2 = i.eval(backBlock, (env, new mutable.Stack))
  println(env2)
//  println(i.backStack)
}


