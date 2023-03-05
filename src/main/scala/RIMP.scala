import rimp.Interpreter
import scala.io.Source

object RIMP extends App {
  val i = new Interpreter()
  println("Welcome to RIMP Interpreter!")
  println("Type help to see possible commands and options.")
  var exit = false
  while (!exit) {
    val input = scala.io.StdIn.readLine()
    if (input.isEmpty) println("Error: Please enter a command or type 'help' for available options.")
    else {
      val parts = input.split("\\s+")
      val command = parts.head
      val args = parts.tail.toList
        command match {
          case "translate" => translate(args)
          case "invert" => invert(args)
          case "evaluate" => evaluate(args)
          case "reverse" => reverse(args)
          case "help" => help()
          case "exit" => exit = true
          case _ => println(s"Unknown command: $command")
        }
    }
    i.resetCounters
  }


  def translate(args: List[String]): Unit = {
    if (args.isEmpty)
      println("Error: please type a file name e.g. 'EX1'")
    else {
      val code = readFile(args.head)
      if (code.startsWith("_Error")) println(code.tail)
      else println(i.translate(code))
    }
  }

  def invert(args: List[String]): Unit = {
    if (args.isEmpty)
      println("Error: please type a file name e.g. 'EX1'")
    else {
      val code = readFile(args.head)
      if (code.startsWith("_Error")) println(code.tail)
      else println(i.translateRev(code))
    }
  }

  def getOption(args: List[String]): String = args match {
    case Nil => ""
    case "--steps"::_ => "steps"
    case _ => "_Error: available options is: '--steps'"
  }

  def evaluate(args: List[String]): Unit = {
    if (args.isEmpty)
      println("Error: please type a file name e.g. 'EX1'")
    else {
      val code = readFile(args.head)
      if (code.startsWith("_Error")) println(code.tail)
      else {
        val option = getOption(args.tail)
        val ast = i.parse(code)

          if (option.startsWith("_Error")){
          println(option.tail)
        }else if (option.equals("steps")){
          val env = i.eval(ast, printSteps = true)
          println(i.stack_tops(env))
        }else{
          val env = i.eval(ast)
          println(i.stack_tops(env))
        }
      }
    }
  }

  def reverse(args: List[String]): Unit = {
    if (args.isEmpty)
      println("Error: please type a file name e.g. 'EX1'")
    else {
      val code = readFile(args.head)
      if (code.startsWith("_Error")) println(code.tail)
      else {
        val option = getOption(args.tail)
        val ast = i.parse(code)
        val env = i.eval(ast)

       if (option.startsWith("_Error")) {
          println(option.tail)
        } else if (option.equals("steps")) {
         val rev_env = i.revEval(ast, env, printSteps = true)
         println(i.stack_tops(rev_env))
        } else {
         val rev_env = i.revEval(ast, env)
         println(i.stack_tops(rev_env))
        }

      }
    }
  }
  def help(): Unit = {

  }


  def readFile(name: String): String = {
    val filename = s"src/main/scala/Examples/$name.rimp"
    try {
      val source = Source.fromFile(filename)
      val output = source.mkString
      source.close()
      if(output.isEmpty) "_Error reading file: file is empty"
      else output
    } catch {
      case ex: Exception =>
        s"_Error reading file: ${ex.getMessage}"
    }
  }

}


