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
      val parameters = parts.tail.toList
        command match {
          case "translate" => translate(parameters)
          case "reverse" => reverse(parameters)
          case "evaluate" => evaluate(parameters)
          case "invert" => invert(parameters)
          case "help" => help()
          case "exit" => exit = true
          case _ => println(s"Unknown command: $command")
        }
    }
    i.resetCounters
  }


  def translate(parameters: List[String]) = {
    if (parameters.isEmpty)
      println("Error: please type a file name e.g. 'EX1'")
    else {
      val code = readFile(parameters.head)
      if (code.startsWith("_Error")) println(code.tail)
      else println(i.translate(code))
    }
  }

  def invert(parameters: List[String]) = {
    if (parameters.isEmpty)
      println("Error: please type a file name e.g. 'EX1'")
    else {
      val code = readFile(parameters.head)
      if (code.startsWith("_Error")) println(code.tail)
      else println(i.translate(code))
    }
  }

  def evaluate(parameters: List[String]) = {
    if (parameters.isEmpty)
      println("Error: please type a file name e.g. 'EX1'")
    else {
      val code = readFile(parameters.head)
      //    val option = parameters(1)
      if (code.startsWith("_Error")) println(code.tail)
      else {
        val ast = i.parse(code)
        val env = i.eval(ast)
        println(i.stack_tops(env))
      }
    }
  }

  def reverse(parameters: List[String]) = {
    if (parameters.isEmpty)
      println("Error: please type a file name e.g. 'EX1'")
    else {
      val code = readFile(parameters.head)
      //    val option = parameters(1)
      if (code.startsWith("_Error")) println(code.tail)
      else {
        val ast = i.parse(code)
        val env = i.eval(ast)
        val rev_env = i.revEval(ast, env)
        println(i.stack_tops(rev_env))
      }


    }
  }
  def help() = {

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


