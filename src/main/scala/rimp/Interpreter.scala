package rimp

import scala.collection.mutable

class Interpreter extends Parser {

  // ------------ RIMP.Interpreter -------------------

  // an interpreter for the RIMP language
  type Env = Map[String, Any]


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


  def eval_bexp(b: BExp, env: Env): Boolean = b match {
    case Bop("=", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
    case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
    case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
    case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
    case Bop(">=", a1, a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
    case Bop("<=", a1, a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
    case Not(b) => !eval_bexp(b, env)
  }

  def eval_stmt(s: Stmt, env: Env, printSteps: Boolean = false): Env =
    s match {
      case Skip =>
        if(printSteps) printEnv(env)
        env
      case Assign(x, a) =>
        val old_stack = env.getOrElse(x, stack())
        val out = env + (x -> old_stack.asInstanceOf[RVar].push(eval_aexp(a, env)))
        if(printSteps) printEnv(out)
        out
      case AssignArr(id, values) =>
        val old_stack = env.getOrElse(id, mutable.Stack(Array.fill(values.length)(stack()))).asInstanceOf[RArray]
        old_stack.push(values.map(v => stack(eval_aexp(v, env))))
        val out = env + (id -> old_stack)
        if(printSteps) printEnv(out)
        out
      case AssignNewArrWithSize(id, size) =>
        val length = eval_aexp(size, env)
        val old_stack = env.getOrElse(id, mutable.Stack(Array.fill(length)(stack()))).asInstanceOf[RArray]
        old_stack.push(Array.fill(length)(stack()))
        val out = env + (id -> old_stack)
        if(printSteps) printEnv(out)
        out
      case UpdateArrIndex(id, index, newVal) =>
        val newVal_eval = eval_aexp(newVal, env)
        val index_eval = eval_aexp(index, env)
        val arr = env(id).asInstanceOf[RArray]
        arr.top(index_eval).push(newVal_eval)
        if(printSteps) printEnv(env)
        env
      case If(b, bl1, bl2, if_id) =>
        if (eval_bexp(b, env)) {
         val out = eval_bl(bl1, env, printSteps)
          if(printSteps) printEnv(out)
          out
        } else {
          val out = eval_bl(bl2, env, printSteps)
          if(printSteps) printEnv(out)
          out
        }

      case While(b, bl, counter) =>
        if (eval_bexp(b, env)) {
            eval_stmt(While(b, bl, counter), eval_bl(bl, env, printSteps), printSteps)
        }
        else {
          if(printSteps) printEnv(env)
          env
        }


      case RevAssign(x, _) =>
        val old_stack = env(x).asInstanceOf[RVar]
        old_stack.pop
        val out = env + (x -> old_stack)
        if (printSteps) printEnv(out)
        out
      case RevAssignArr(id, _) =>
        val old_stack = env(id).asInstanceOf[RArray]
        old_stack.pop()
        val out = env + (id -> old_stack)
        if (printSteps) printEnv(out)
        out
      case RevAssignNewArrWithSize(id, _) =>
        val old_stack = env(id).asInstanceOf[RArray]
        old_stack.pop()
        val out = env + (id -> old_stack)
        if (printSteps) printEnv(out)
        out
      case RevUpdateArrIndex(id, index, _) =>
        val index_eval = eval_aexp(index, env)
        val arr = env(id).asInstanceOf[RArray].top
        arr(index_eval).pop
        if (printSteps) printEnv(env)
        env
    }

  def eval_bl(bl: Block, env: Env, printSteps: Boolean = false): Env = bl match {
    case Nil => env
    case s :: bl =>
     eval_bl(bl, eval_stmt(s, env, printSteps), printSteps)
  }

  def eval(bl: Block, env: Env = Map(), printSteps: Boolean = false): Env = {
    if(printSteps) printEnv(env)
    eval_bl(bl, env, printSteps)
  }

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


  def printEnv(env: Env): Unit = {
    println(env.map { case (key, value) =>
      value match {
        case s: mutable.Stack[_] if s.head.isInstanceOf[Array[RVar]] =>
          s"$key -> ${s.map(a => s"Array${a.asInstanceOf[Array[RVar]].map(a => s"${a.toString}").mkString("(", ", ", ")")}")}"
        case rest => s"$key -> ${rest.toString}"
      }
    }.mkString("Map(", ", ", ")"))
  }

}