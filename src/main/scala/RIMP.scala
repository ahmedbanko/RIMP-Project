import rimp.Interpreter

object RIMP extends App {
  val i = new Interpreter()
//  println("Welcome to RIMP Interpreter!")
//  println("Type help to see possible commands and options.")
//  var exit = false
//  while (!exit) {
//    val input = scala.io.StdIn.readLine()
//    if (input.isEmpty) println("Error: Please enter a command or type 'help' for available options.")
//    else {
//      val parts = input.split("\\s+")
//      val command = parts.head
//      val args = parts.tail.toList
//      try {
//        command match {
//          case "translate" => translate(args)
//          case "invert" => invert(args)
//          case "evaluate" => evaluate(args)
//          case "reverse" => reverse(args)
//          case "help" => help()
//          case "exit" => exit = true
//          case _ => println(s"Unknown command: $command")
//        }
//      } catch {
//        case e: java.util.NoSuchElementException => println("Syntax error: the program code is not formed correctly")
//        case e: Exception => println(e)
//      }
//
//    }
//    i.resetCounters
//  }
//
//
//  def translate(args: List[String]): Unit = {
//    if (args.isEmpty)
//      println("Error: please type a file name e.g. 'EX1'")
//    else {
//      val code = readFile(args.head)
//      if (code.startsWith("_Error")) println(code.tail)
//      else println(i.translate(code))
//    }
//  }
//
//  def invert(args: List[String]): Unit = {
//    if (args.isEmpty)
//      println("Error: please type a file name e.g. 'EX1'")
//    else {
//      val code = readFile(args.head)
//      if (code.startsWith("_Error")) println(code.tail)
//      else println(i.invert(code))
//    }
//  }
//
//  def getOption(args: List[String]): String = args match {
//    case Nil => ""
//    case "--steps"::_ => "steps"
//    case _ => "_Error: available options is: '--steps'"
//  }
//
//  def evaluate(args: List[String]): Unit = {
//    if (args.isEmpty)
//      println("Error: please type a file name e.g. 'EX1'")
//    else {
//      val code = readFile(args.head)
//      if (code.startsWith("_Error")) println(code.tail)
//      else {
//        val option = getOption(args.tail)
//        val ast = i.parse(code)
//
//          if (option.startsWith("_Error")){
//          println(option.tail)
//        }else if (option.equals("steps")){
//          val env = i.eval(ast, printSteps = true)
//          println(i.stack_tops(env))
//        }else{
//          val env = i.eval(ast)
//          println(i.stack_tops(env))
//        }
//      }
//    }
//  }
//
//  def reverse(args: List[String]): Unit = {
//    if (args.isEmpty)
//      println("Error: please type a file name e.g. 'EX1'")
//    else {
//      val code = readFile(args.head)
//      if (code.startsWith("_Error")) println(code.tail)
//      else {
//        val option = getOption(args.tail)
//        val ast = i.parse(code)
//        val env = i.eval(ast)
//
//        val rev_ast = i.revAST(ast)
//       if (option.startsWith("_Error")) {
//          println(option.tail)
//        } else if (option.equals("steps")) {
//         val rev_env = i.eval(rev_ast, env, printSteps = true)
//         println(i.stack_tops(rev_env))
//        } else {
//         val rev_env = i.eval(rev_ast, env)
//         println(i.stack_tops(rev_env))
//        }
//
//      }
//    }
//  }
//  def help(): Unit = {
//    println("Type one of the following commands in the format COMMAND <file_name> --OPTION")
//    println("'translate' prints the given code with additional variables/counters to it.")
//    println("'invert' prints reversed representation of the given code.")
//    println("'evaluate' executes the code in the forward direction and prints the final values of all variables.")
//    println("'reverse' executes the code in the backward direction and prints the final values of all variables.")
//    println("NOTE: the commands 'evaluate' & 'reverse' can run with the option '--steps' which prints the runtime values.")
//  }
//
//
//  def readFile(name: String): String = {
//    val filename = s"src/main/scala/Examples/$name.rimp"
//    try {
//      val source = Source.fromFile(filename)
//      val output = source.mkString
//      source.close()
//      if(output.isEmpty) "_Error reading file: file is empty"
//      else output
//    } catch {
//      case ex: Exception =>
//        s"_Error reading file: ${ex.getMessage}"
//    }
//  }

  val EX1 =
    """
       fact := 1;
       n := 3;
       while (!n > 0) do {
            fact := !n * !fact;
            n := !n - 1
       }
      """

  val EX2 =
    """a := 49;
              b := 28;
              while ~(!a = !b) do
              if !a > !b
              then a := !a - !b
              else b := !b - !a"""

  val EX3 =
    """x := 12;
  while !x > 1 do {
      r := !x;
      while !r > 1 do {
          r := !r - 2
          };
      if !r = 0
      then x := !x / 2
      else x := 3 * !x + 1}"""


  val EX4 =
    """x := 13;
  factor := 2;
  isprime := 1;
  limit := !x / 2 + 1;
  while !factor < !limit do {
      r := !x;
      (while !r > !factor - 1 do
          r := !r - !factor);
      (if !r = 0 then isprime := 0 else skip);
      factor := !factor + 1}"""

  val EX5 =
    """n := 10;
  a := 1;
  b := 1;
  i := 0;
  while !i < !n do {
      tmp := !a;
      a := !b;
      b := !tmp + !a;
      i := !i + 1}"""


  val arrProg1 =
    """arr := |10|;
       i := 0;
       while (!i < 10) do {
        arr[!i] := !i;
        i := !i + 1
       };
       ii := 0;
       while (!ii < 10) do {
          x := arr[!ii];
            ii := !ii + 1
           }"""

  val arrProg2 = "arr := [1+1, 2, 3]; i1before := arr[1]; arr[1] := 10; i1after := arr[1]"


  val if_true_prog =
    """
      x := 10;
      y := 100;
      if (!x > 0) then {
          x := !x - 1
      }else{
          y := !y - 1
      }
      """

  val if_false_prog =
    """
      x := 10;
      y := 100;
      if (!x > 100) then {
          x := !x - 1
      }else{
          y := !y - 1
      }
      """


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

  val env = i.eval(i.parse(reverse_arr_prog))
  println(i.env2string(env))

}


