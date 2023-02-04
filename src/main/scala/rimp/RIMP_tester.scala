package rimp

class RIMP_tester extends Interpreter {

  val exampleProg1 =
    """fact := 1;
    n := 3;
    while (!n > 0) do {
        fact := !n * !fact;
        n := !n - 1}
        """

  val exampleProg2 = """a := 49;
                b := 28;
                while ~(!a = !b) do
                if !a > !b
                then a := !a - !b
                else b := !b - !a""".stripMargin

  val exampleProg3 =
    """x := 12;
    while !x > 1 do {
        r := !x;
        (while !r > 1 do
            r := !r - 2);
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


  def testLexer(): Unit = {
    println("# Lexing if (!a < !b) then skip else a := !a * !b + 1")
    println(lexing_simp(RIMP_REGS, "if (!a < !b) then skip else a := !a * !b + 1").filter(_._1 != "whitespace"))
    println("----------------------------------------------")

    println("# Lexing exampleProg1")
    println(lexing_simp(RIMP_REGS, exampleProg1).filter(_._1 != "whitespace"))
    println("----------------------------------------------")

    println("# Lexing exampleProg2")
    println(lexing_simp(RIMP_REGS, exampleProg2).filter(_._1 != "whitespace"))
    println("----------------------------------------------")

    println("# Lexing exampleProg3")
    println(lexing_simp(RIMP_REGS, exampleProg3).filter(_._1 != "whitespace"))
    println("----------------------------------------------")

    println("# Lexing exampleProg4")
    println(lexing_simp(RIMP_REGS, exampleProg4).filter(_._1 != "whitespace"))
    println("----------------------------------------------")

    println("# Lexing exampleProg5")
    println(lexing_simp(RIMP_REGS, exampleProg5).filter(_._1 != "whitespace"))
    println("----------------------------------------------")

  }

  def testParser(): Unit = {
    println("""Parsing "if (!a < !b) then skip else a := !a * !b + 1"""")
    println(parse("if (!a < !b) then skip else a := !a * !b + 1"))
    println("------------------------------------------------")

    println("Parsing exampleProg1")
    println(parse(exampleProg1))
    println("------------------------------------------------")

    println("Parsing exampleProg2")
    println(parse(exampleProg2))
    println("------------------------------------------------")

    println("Parsing exampleProg3")
    println(parse(exampleProg3))
    println("------------------------------------------------")

    println("Parsing exampleProg4")
    println(parse(exampleProg4))
    println("------------------------------------------------")

    println("Parsing exampleProg5")
    println(parse(exampleProg5))
    println("------------------------------------------------")

  }


  def testEvaluate(): Unit = {
    println("""Evaluating "exampleProg1"""")
    println(eval(parse(exampleProg5)))
    println("------------------------------------------------")
  }

}
