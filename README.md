# User Guide

Users can run the user interface to interact and experiment with the RIMP interpreter implemented in this project, in different ways. Below we will provide instructions for two ways in which users can launch our system, as well as instructions on how to use the interpreter for executing code written in RIMP language. Note that Java and Scala are assumed to be installed (the suggested Scala version is Scala 2.13.10).

## Starting the System Using Source Code

The zip file provided with this report includes a directory named ”RIMP-Project”. This directory includes all source code used to implement our system, as well as the test classes used to test the software.

### Using any IDE:

1. Make sure Scala plugin is installed.
2. Navigate to the project menu.
3. Choose the option to import a project.
4. Locate where the directory ”RIMP-Project” is and select it.
5. Resolve dependencies if needed.
6. Compile the project.
7. Run the project from ”RIMP.scala” class which includes the main method.

## Starting the System Using Jar File

An alternative way to run our RIMP interpreter is through the jar file provided as a part of the zip file. Note that the files including the source code written in RIMP must have the extension ”.rimp” and must be in the same directory of the jar file.

### Instructions:

1. Move the jar file named ”RIMP.jar” to any desired directory.
2. Open a terminal or command prompt.
3. Navigate to the directory where the jar file is located.
4. Type the command `java -jar RIMP.jar` and press Enter.

## Using RIMP Interpreter

Once the system is launched using one of the ways explained above, a REPL will start in the terminal printing the welcome message:


Then commands of the form `command file-name` can be used. The available commands are listed below:

- `help`: prints details about the different available commands and instructions for how to use the interpreter.
- `translate`: prints the given code with additional variables/counters to it.
- `invert`: prints reversed representation of the given code.
- `evaluate`: executes the code in the forward direction and prints the final values of all variables.
- `reverse`: executes the code in the backward direction and prints the final values of all variables.
- `exit`: terminates the system.

**Note:** all commands are written in lowercase letters and commands `evaluate` & `reverse` can run with the option `--steps` which prints the run-time values.
