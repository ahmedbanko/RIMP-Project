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
//  val parsed = i.parse(prog)
//  val (env, bStack) = i.eval(parsed)
//  println(env("a"))
//  println(env("arr").asInstanceOf[Array[Int]].mkString("Array(", ", ", ")"))
//  println(bStack)
//  println(parsed)
//  println(i.rev(parsed))
////  println(bStack)
//  val backBlock : i.Block = bStack.map(e => e._2).toList
//  val (env2, bStack2) = i.eval(backBlock, (Map().empty, new mutable.Stack))
//  println(env2("a"))
//  println(env2("arr").asInstanceOf[Array[Int]].mkString("Array(", ", ", ")"))
//  println(bStack2)
//  println(i.backStack)
//  val (env2, bStack2) = i.revEval((env, bStack))
//  println(env2("a"))
//  println(env2("arr").asInstanceOf[Array[Int]].mkString("Array(", ", ", ")"))

  val ifProg =
    """x := 1;
    y := 2;
    if (!x < !y) then {
    x := 0;
    if ~(!x > !y) then {
    x := 22
    } else {
    x := 99
    }
    } else {
    x := 11
    }
      """
  val exampleProg1 =
    """
       fact := 1;
       n := 3;
       while (!n > 0) do {
            fact := !n * !fact;
            n := !n - 1
       }
      """

  val exampleProg2 =
    """a := 49;
              b := 28;
              while ~(!a = !b) do
              if !a > !b
              then a := !a - !b
              else b := !b - !a""".stripMargin

  val exampleProg3 =
    """x := 12;
  while !x > 1 do {
      r := !x;
      while !r > 1 do {
          r := !r - 2
          };
      if !r = 0
      then x := !x / 2
      else x := 3 * !x + 1}""".stripMargin


  val exampleProg4 =
    """x := 13;
  factor := 2;
  isprime := 1;
  limit := !x / 2 + 1;
  while !factor < !limit do {
      r := !x;
      (while !r > !factor - 1 do
          r := !r - !factor);
      (if !r = 0 then isprime := 0 else skip);
      factor := !factor + 1}""".stripMargin

  val exampleProg5 =
    """n := 10;
  a := 1;
  b := 1;
  i := 0;
  while !i < !n do {
      tmp := !a;
      a := !b;
      b := !tmp + !a;
      i := !i + 1}""".stripMargin

//  val reversedShouldBe =
//    """while-0 (!k_0 > 0) do {
//          k_0 =: (!k_0 + 1);
//          n =: !n - 1;
//          fact =: !n * !fact;
//          };
//       k_0 =: 0;
//       n =: 3;
//       fact =: 1
//        """
//  if(!n > 0) then {
//    n := 1
//  }else{
//    n := 2
//  }

  val stacks =
    """n := 10;
       while(!n > 0) do {
            n := !n - 1
       }
      """.stripMargin
  val p = i.parse(exampleProg2)
  p.foreach(println)
  println("------------")
  val rp = i.revAST(p)
  println(i.ast2Code(p))
  println("------------")
  println(i.revAst2Code(p))
//  val env = i.eval(p)
//  println(i.stack_top_map(env))
//  val reenv = i.revEval(rp, env)
//  println(i.stack_top_map(reenv))
//// env("arr").asInstanceOf[Array[mutable.Stack[Int]]].foreach(x => println(x.mkString(", ")))
//// env("arr2").asInstanceOf[Array[mutable.Stack[Int]]].foreach(x => println(x.mkString(", ")))
////  println(env)
////  println(i.print_top(env))
  println("done")
//
//  //  i.evalBySteps(p, Map())


  """
     Map(fact -> 6, n -> 0, W_1_k -> 3)
    """.stripMargin




}


