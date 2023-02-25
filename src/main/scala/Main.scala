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
       while(!l > 0) do {
            i := !l - 1;
            arr[!i] := !n;
            l := !l - 1;
            n := !n + 1
       };
       swapped := 1;
       while(!swapped = 1) do {
            swapped := 0;
            j := !n - 1;
            tmp1 := arr[!j];
            tmp2 := arr[!n];
          if(!tmp1 > !tmp2) then {
               arr[!j] := !tmp2;
               arr[!n] := !tmp1;
               swapped := 1
          }else {
            skip
          }
       }
      """.stripMargin

  val reverse_arr_prog =
    """arr := [1, 2, 3, 4, 5];
       left := 0;
       right := 4;
       while(!left < !right) do {
          tmp_left := arr[!left];
          tmp_right := arr[!right];
          arr[!right] := !tmp_left;
           arr[!left] := !tmp_right;
          left := !left + 1;
          right := !right - 1
       }
      """
  val p = i.parse(reverse_arr_prog)
//  p.foreach(println)
//  println("------------")
  val rp = i.revAST(p)
//  println(i.ast2Code(p))
////  println("------------")
////  println(i.revAst2Code(p))
  val env = i.eval(p)
  println(i.stack_tops(env))
////  println(rp)
  val revenv = i.revEval(rp, env)
  println(i.stack_tops(revenv))
//
//  println(i.stack(1, 2))

//  println(i.lexing_simp(i.RIMP_REGS, "n := 10"))
  println("done")
//
//  //  i.evalBySteps(p, Map())





}


