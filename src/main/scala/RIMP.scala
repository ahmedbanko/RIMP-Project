

object RIMP extends App {
//  val p = new Parser()
//  val i = new Interpreter()
  val prog =
    """a := 10;
       a := 9;
       a := 8;
       arr := |1|;
       arr := |2|;
       arr := |3|"""

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
    """n := 2;
       while(!n > 0) do {
           n := !n - 1;
           n2 := 10;
           while(!n2 > 0) do {
                n2 := !n2 - 1
           }
       }
      """

  val if_prog =
    """
      x := 10;
      y := 100;
      if (!x > 0) then {
          x := !x - 1
      }else{
          skip
      }
      """

  val sort_arr_prog =
    """n := 0;
       l := 5;
       arr := |!l|;
       while(!l + -1 > 0) do {
            arr[!l - 1] := !n;
            l := !l - 1;
            n := !n + 1
       }
      """.stripMargin

  val reverse_arr_prog =
    """arr := [1, 2, 3, 4, 5];
       arr_len := 5;
       left := 0;
       right := !arr_len - 1;
       while(!left < !right) do {
          tmp_left := arr[!left];
          tmp_right := arr[!right];
          arr[!right] := !tmp_left;
           arr[!left] := !tmp_right;
          left := !left + 1;
          right := !right - 1
       }
      """

  val thread_prog =
    """thread t1 := {
        arr := [1, 2, 3, 4, 5];
          arr_len := 5;
          left := 0;
          right := !arr_len - 1;
          while(!left < !right) do {
             tmp_left := arr[!left];
             tmp_right := arr[!right];
             arr[!right] := !tmp_left;
              arr[!left] := !tmp_right;
             left := !left + 1;
             right := !right - 1
          }
       };
       run ?t1;
       thread t2 := {
            fact := 1;
           n := 3;
           while (!n > 0) do {
                fact := !n * !fact;
                n := !n - 1
           }
           };
           run ?t2
     """
//  val p = i.parse(stacks)
////  p.foreach(println)
////  println("------------")
//  val rp = i.revAST(p)
////  println(i.ast2Code(p))
//////  println("------------")
//////  println(i.revAst2Code(p))
//  val env = i.eval(p)
//  println(i.stack_tops(env))
//////  println(rp)
//  val revenv = i.revEval(rp, env)
//  println(i.stack_tops(revenv))
////
////  println(i.stack(1, 2))
//
////  println(i.lexing_simp(i.RIMP_REGS, "n := 10"))
//  println("done")
//
//  //  i.evalBySteps(p, Map())


//  EX2
//  (7, +(-7, +(-7, +(-28, +(49, +(0, 0)))))
//  (14, +(-7, +(-28, +(49, +(0, 0)))))
//  (21, +(-28, +(49, +(0, 0))))
//  (49, +(49, +(0, 0)))
//  (0, +(0, 0))



  println("Welcome to RIMP Interpreter!")
  println("Type help to see possible commands and options.")
  var exit = false
  while (!exit) {
    val input = scala.io.StdIn.readLine()
    val parts = input.split("\\s+")
    val command = parts.head
    val parameters = parts.tail
    print(s"command: $command ")
    command match {
      case "translate" => translate(parameters.toList)
      case "reverse" => reverse(parameters.toList)
      case "evaluate" => evaluate(parameters.toList)
      case "invert" => invert(parameters.toList)
      case "help" => help(parameters.toList)
      case "exit" => exit = true
      case _ => println(s"Unknown command: $command")
    }
  }


  def translate(parameters: List[String]) = {
    parameters.foreach(x => print(s"$x "))
    println()
  }

  def reverse(parameters: List[String]) = {
    parameters.foreach(x => print(s"$x "))
    println()
  }

  def evaluate(parameters: List[String]) = {
    parameters.foreach(x => print(s"$x "))
    println()
  }

  def invert(parameters: List[String]) = {
    parameters.foreach(x => print(s"$x "))
    println()
  }
  def help(parameters: List[String]) = {
    parameters.foreach(x => print(s"$x "))
    println()
  }

}


