import rimp.Interpreter

import scala.io.Source

object RIMP extends App {
  override def main(args: Array[String]): Unit = {
    val i = new Interpreter()
    println("Welcome to RIMP Interpreter!")
    println("Type 'help' to see possible commands and options.")
    var exit = false
    while (!exit) {
      val input = scala.io.StdIn.readLine()
      if (input.isEmpty) println("Error: Please enter a command or type 'help' for available options.")
      else {
        val parts = input.split("\\s+")
        val command = parts.head
        val args = parts.tail.toList
        try {
          command match {
            case "translate" => translate(args)
            case "invert" => invert(args)
            case "evaluate" => evaluate(args)
            case "reverse" => reverse(args)
            case "help" => help()
            case "exit" => exit = true
            case _ => println(s"Unknown command: $command")
          }
        } catch {
          case e: java.util.NoSuchElementException => println("Syntax error: the program code is not formed correctly")
          case e: Exception => println(e)
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
        else println(i.invert(code))
      }
    }

    def getOption(args: List[String]): String = args match {
      case Nil => ""
      case "--steps" :: _ => "steps"
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

          if (option.startsWith("_Error")) {
            println(option.tail)
          } else if (option.equals("steps")) {
            val env = i.eval(ast, printSteps = true)
            println(i.stack_tops(env))
          } else {
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

          val rev_ast = i.revAST(ast)
          if (option.startsWith("_Error")) {
            println(option.tail)
          } else if (option.equals("steps")) {
            val rev_env = i.eval(rev_ast, env, printSteps = true)
            println(i.stack_tops(rev_env))
          } else {
            val rev_env = i.eval(rev_ast, env)
            println(i.stack_tops(rev_env))
          }

        }
      }
    }

    def help(): Unit = {
      println("Type one of the following commands in the format COMMAND <file_name> --OPTION")
      println("'translate' prints the given code with additional variables/counters to it.")
      println("'invert' prints reversed representation of the given code.")
      println("'evaluate' executes the code in the forward direction and prints the final values of all variables.")
      println("'reverse' executes the code in the backward direction and prints the final values of all variables.")
      println("'exit' terminates the system.")
      println("NOTE: the commands 'evaluate' & 'reverse' can run with the option '--steps' which prints the runtime values.")
    }


    def readFile(name: String): String = {
      val filename = s"$name.rimp"
      try {
        val source = Source.fromResource(filename)
        val output = source.mkString
        source.close()
        if (output.isEmpty) "_Error reading file: file is empty"
        else output
      } catch {
        case ex: Exception =>
          s"_Error reading file: file ${filename} not found"
      }
    }

  }
}
