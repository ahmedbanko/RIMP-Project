package rimp

import scala.collection.mutable

/**
 * Class Interpreter.
 *
 * Interpreter class includes implementation of functions which
 * evaluates various type of statements and expressions using Scala
 * programming language. The code used in this
 * class is developed from the source code given in Compiler and
 * Formal Languages Module at King's College London led by Dr. Christian Urban.
 */

class Interpreter extends Parser {

  // ------------ RIMP.Interpreter -------------------

  // an interpreter for the RIMP language
  type Env = Map[String, Any]

  /**
   * Evaluates an arithmetic expression in the given environment and returns the result.
   *
   * @param a The arithmetic expression to evaluate.
   * @param env The environment in which to evaluate the expression.
   * @return The result of evaluating the expression.
   */
  def eval_aexp(a: AExp, env: Env): Int = a match {
    case Num(i) => i
    case Var(s) => env(s).asInstanceOf[RVar].top.value
    case ArrayVar(id, index) =>
      val valsArray = env(id).asInstanceOf[RArray].top
      val indexVal = eval_aexp(index, env)
      valsArray(indexVal).top.value
    case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
    case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
    case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
    case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
    case Aop("%", a1, a2) => eval_aexp(a1, env) % eval_aexp(a2, env)
  }

  /**
   * Evaluates a boolean expression in the given environment and returns the result.
   *
   * @param b The boolean expression to evaluate.
   * @param env The environment in which to evaluate the expression.
   * @return The result of evaluating the expression.
   */
  def eval_bexp(b: BExp, env: Env): Boolean = b match {
    case Bop("=", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
    case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
    case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
    case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
    case Bop(">=", a1, a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
    case Bop("<=", a1, a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
    case Not(b) => !eval_bexp(b, env)
  }

  /**
   * Evaluates a given statement in the given environment.
   * Includes evaluation of both forward and backwards assignment statements.
   *
   * @param s the statement to evaluate
   * @param env the environment to evaluate the statement in
   * @param printSteps if true, prints each step of the evaluation process to the console
   * @return the updated environment after executing the statement
   */
  def eval_stmt(s: Stmt, env: Env, printSteps: Boolean = false): Env =
    s match {
      case Skip =>
        if(printSteps) println(env2string(env))
        env
      case Assign(x, a) =>
        val old_stack = env.getOrElse(x, stack())
        val out = env + (x -> old_stack.asInstanceOf[RVar].push(eval_aexp(a, env)))
        if(printSteps) println(env2string(out))
        out
      case AssignArr(id, values) =>
        val old_stack = env.getOrElse(id, mutable.Stack(Array.fill(values.length)(stack()))).asInstanceOf[RArray]
        old_stack.push(values.map(v => stack(eval_aexp(v, env))))
        val out = env + (id -> old_stack)
        if(printSteps) println(env2string(out))
        out
      case AssignNewArrWithSize(id, size) =>
        val length = eval_aexp(size, env)
        val old_stack = env.getOrElse(id, mutable.Stack(Array.fill(length)(stack()))).asInstanceOf[RArray]
        old_stack.push(Array.fill(length)(stack()))
        val out = env + (id -> old_stack)
        if(printSteps) println(env2string(out))
        out
      case UpdateArrIndex(id, index, newVal) =>
        val newVal_eval = eval_aexp(newVal, env)
        val index_eval = eval_aexp(index, env)
        val arr = env(id).asInstanceOf[RArray]
        arr.top(index_eval).push(newVal_eval)
        if(printSteps) println(env2string(env))
        env
      case If(b, bl1, bl2, if_id) =>
        if (eval_bexp(b, env)) {
         val out = eval_bl(bl1, env, printSteps)
          if(printSteps) println(env2string(out))
          out
        } else {
          val out = eval_bl(bl2, env, printSteps)
          if(printSteps) println(env2string(out))
          out
        }

      case While(b, bl, counter) =>
        if (eval_bexp(b, env)) {
            eval_stmt(While(b, bl, counter), eval_bl(bl, env, printSteps), printSteps)
        }
        else {
          if(printSteps) println(env2string(env))
          env
        }

      case RevAssign(x, _) =>
        val old_stack = env(x).asInstanceOf[RVar]
        old_stack.pop
        val out = env + (x -> old_stack)
        if (printSteps) println(env2string(out))
        out
      case RevAssignArr(id, _) =>
        val old_stack = env(id).asInstanceOf[RArray]
        old_stack.pop()
        val out = env + (id -> old_stack)
        if (printSteps) println(env2string(out))
        out
      case RevAssignNewArrWithSize(id, _) =>
        val old_stack = env(id).asInstanceOf[RArray]
        old_stack.pop()
        val out = env + (id -> old_stack)
        if (printSteps) println(env2string(out))
        out
      case RevUpdateArrIndex(id, index, _) =>
        val index_eval = eval_aexp(index, env)
        val arr = env(id).asInstanceOf[RArray].top
        arr(index_eval).pop
        if (printSteps) println(env2string(env))
        env
    }

  /**
   * Evaluate a block of statements in the given environment.
   *
   * @param bl The block of statements to evaluate.
   * @param env The environment in which to evaluate the statements.
   * @param printSteps Whether to print each evaluation step or not.
   * @return The updated environment after evaluating the block of statements.
   */
  def eval_bl(bl: Block, env: Env, printSteps: Boolean = false): Env = bl match {
    case Nil => env
    case s :: bl =>
     eval_bl(bl, eval_stmt(s, env, printSteps), printSteps)
  }

  /**
   * Evaluates the given block of code in the specified environment.
   *
   * @param bl The block of code to evaluate.
   * @param env The environment in which to evaluate the code (default is an empty map).
   * @param printSteps Indicates whether to print the intermediate steps of evaluation (default is false).
   * @return The environment after evaluating the code.
   */
  def eval(bl: Block, env: Env = Map(), printSteps: Boolean = false): Env = {
    if(printSteps) println(env2string(env))
    eval_bl(bl, env, printSteps)
  }

  /**
   * Returns a string representation of the top elements of all stacks in the given environment.
   *
   * @param env The environment to inspect.
   * @return A string representation of the top elements of all stacks in the environment.
   */
  def stack_tops(env: Env): String = {
    env.map { case (key, value) =>
      value match {
        case i: Int => s"$key -> $i"
        case s: RVar => s"$key -> ${s.top.value}"
        case s: mutable.Stack[_] if s.head.isInstanceOf[Int] => s"$key -> ${s.head}"
        case s: mutable.Stack[_] if s.head.isInstanceOf[Array[RVar]] =>
          val top_arr = s.head.asInstanceOf[Array[RVar]]
          s"$key -> Array[${top_arr.map(e => e.top.value).mkString(", ")}]"
      }
    }.mkString("Map(", ", ", ")")
  }

  /**
   * Converts an environment map to a string representation.
   *
   * @param env The environment map to convert.
   * @return A string representation of the environment map.
   */
  def env2string(env: Env): String = {
    env.map { case (key, value) =>
      value match {
        case s: mutable.Stack[_] if s.head.isInstanceOf[Array[RVar]] =>
          s"$key -> ${s.map(a => s"Array${a.asInstanceOf[Array[RVar]].map(a => s"${a.toString}").mkString("(", ", ", ")")}")}"
        case rest => s"$key -> ${rest.toString}"
      }
    }.mkString("Map(", ", ", ")")
  }

}