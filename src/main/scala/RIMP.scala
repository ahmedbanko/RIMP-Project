import rimp.Interpreter

import scala.io.Source

/**
 * The RIMP object is the main entry point of the RIMP Interpreter. It contains the main method which
 * runs the interpreter's command line interface, and a set of command functions to perform different
 * operations on the input code.
 */
object RIMP {
  /**
   * The main method is the entry point of the RIMP interpreter. It prints a welcome message and provides
   * a command line interface to the user to interact with the interpreter.
   *
   * @param args The arguments passed as parameters.
   */
   def main(args: Array[String]): Unit = {
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
       i.resetCounters()
     }

     /**
      * The 'translate' function takes a list of strings as arguments and prints the given code with additional
      *  variables/counters added to it.
      *  @param args The list of command arguments, with the file name as the first element.
      *  @note Prints the error message if file is not provided in the argument list.
      */
     def translate(args: List[String]): Unit = {
       if (args.isEmpty)
         println("Error: please type a file name e.g. 'EX1'")
       else {
         val code = readFile(args.head)
         if (code.startsWith("_Error")) println(code.tail)
         else println(i.translate(code))
       }
     }

     /**
      * The 'invert' function takes a list of strings as arguments and prints the reversed representation of the
      *  given code.
      *  @param args The list of command arguments, with the file name as the first element.
      *  @note Prints the error message if file is not provided in the argument list.
      */
     def invert(args: List[String]): Unit = {
       if (args.isEmpty)
         println("Error: please type a file name e.g. 'EX1'")
       else {
         val code = readFile(args.head)
         if (code.startsWith("_Error")) println(code.tail)
         else println(i.invert(code))
       }
     }

     /**
      * Parses the given arguments and returns the option specified by the user.
      *
      * @param args a list of command line arguments
      * @return the specified option as a string, or an error message if the option is invalid or not specified
      */
     def getOption(args: List[String]): String = args match {
       case Nil => ""
       case "--steps" :: _ => "steps"
       case _ => "_Error: available options is: '--steps'"
     }

     /**
      * Evaluates a RIMP program in the forward direction and prints the final values of all variables.
      *
      * @param args a list of command line arguments
      */
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

     /**
      * Reverses a RIMP program and evaluates it in the backward direction, printing the final values of all variables.
      *
      * @param args a list of command line arguments
      */
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

     /**
      * Prints a list o available commands and how to use them.
      */
     def help(): Unit = {
       println("Type one of the following commands in the format COMMAND <file_name> --OPTION")
       println("'translate' prints the given code with additional variables/counters added to it.")
       println("'invert' prints reversed representation of the given code.")
       println("'evaluate' executes the code in the forward direction and prints the final values of all variables.")
       println("'reverse' executes the code in the backward direction and prints the final values of all variables.")
       println("'exit' terminates the system.")
       println("NOTE: the commands 'evaluate' & 'reverse' can run with the option '--steps' which prints the runtime values.")
     }

     /**
      * Reads the contents of the specified RIMP file and returns them as a string.
      * @param name the name of the file to read
      * @return the contents of the file as a string, or an error message if the file cannot be read
      */
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
