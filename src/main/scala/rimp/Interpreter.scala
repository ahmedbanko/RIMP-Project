package rimp

import scala.collection.mutable

class Interpreter extends Parser {

  // ------------ RIMP.Interpreter -------------------


  // an interpreter for the RIMP language
  type Env = Map[String, Any]


  def eval_aexp(a: AExp, env: Env): Int = a match {
    case Num(i) => i
    case Var(s) => env(s).asInstanceOf[RVar].top
    case ArrayVar(id, index) =>
      val valsList = env(id).asInstanceOf[RArray]
      val indexVal = eval_aexp(index, env)
      valsList(indexVal).top
    case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
    case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
    case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
    case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
    case Aop("%", a1, a2) => eval_aexp(a1, env) % eval_aexp(a2, env)
  }


  def eval_bexp(b: BExp, env: Env): Boolean = b match {
    case True => true
    case False => false
    case Bop("=", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
    case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
    case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
    case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
    case Bop(">=", a1, a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
    case Bop("<=", a1, a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
    case Not(b) => !eval_bexp(b, env)
  }


  def eval_thread(bl: Block, env: Env): Env = {
    new Thread(() => {
      eval_bl(bl, env)
    }).start()
    env
  }

  def eval_stmt(s: Stmt, env: Env): Env =
    s match {
      case Skip => env
      case Assign(x, a) =>
        val old_stack = env.getOrElse(x, stack())
        env + (x -> old_stack.asInstanceOf[RVar].push(eval_aexp(a, env)))
      case AssignArr(id, values) =>
        val old_array = env.getOrElse(id, Array.fill(values.length)(stack())).asInstanceOf[RArray]
        for ((arr, i) <- old_array.zipWithIndex) {
          arr.push(eval_aexp(values(i), env))
        }
        env + (id -> old_array)
      case ArrayWithSize(id, size) =>
        val new_array = Array.fill(eval_aexp(size, env))(stack())
        env + (id -> new_array)
      case UpdateArrIndex(id, index, newVal) =>
        val newVal_eval = eval_aexp(newVal, env)
        val index_eval = eval_aexp(index, env)
        val arr = env(id).asInstanceOf[RArray]
        arr(index_eval).push(newVal_eval)
        env
      case AssignThread(id, bl) => env + (id -> bl)
      case RunThread(id) => eval_thread(env(id).asInstanceOf[Block], env)
      case If(b, bl1, bl2, if_res) =>
        val stack = env.getOrElse(if_res.id, mutable.Stack[Int](0)).asInstanceOf[RVar]
        if (eval_bexp(b, env)) {
          stack.push(1) // represents true
          eval_bl(bl1, env + (if_res.id -> stack))
        } else {
          stack.push(0) // represents false
          eval_bl(bl2, env + (if_res.id -> stack))
        }
      case While(b, bl, counter) =>
        if (eval_bexp(b, env)) {
          eval_stmt(While(b, bl, Counter(counter.id, counter.count+1)), eval_bl(bl, env))
        }
        else {
          val c_stack = env.getOrElse(counter.id, stack()).asInstanceOf[RVar]

          env + (counter.id -> c_stack.push(counter.count))
        }
    }

  def eval_bl(bl: Block, env: Env): Env = bl match {
    case Nil => env
    case s :: bl => eval_bl(bl, eval_stmt(s, env))
  }

  def eval(bl: Block, env: Env = Map()): Env = eval_bl(bl, env)

  def stack_tops(env: Env): String = {
    env.map { case (key, value) =>
      value match {
        case i: Int => s"$key -> $i"
        case s: RVar => s"$key -> ${s.top}"
        case s_l: RArray =>
          s"$key -> Array[${s_l.map(s => s.top).mkString(", ")}]"
      }
    }.mkString("Map(", ", ", ")")
  }


  def revEval_stmt(s: Stmt, env: Env): Env =
    s match {
      case Skip => env
      case Assign(x, _) =>
        val old_stack = env(x).asInstanceOf[RVar]
        old_stack.pop
        env + (x -> old_stack)
      case AssignArr(id, _) =>
        val old_array = env(id).asInstanceOf[RArray]
        for ((arr, _) <- old_array.zipWithIndex) {
          if(arr.size > 1) arr.pop
        }
        env + (id -> old_array)
      case ArrayWithSize(id, _) =>
        val old_array = env(id).asInstanceOf[RArray]
        for ((arr, _) <- old_array.zipWithIndex) {
          if (arr.size > 1) arr.pop
        }
        env + (id -> old_array)
      case UpdateArrIndex(id, index, _) =>
        val index_eval = eval_aexp(index, env)
        val arr = env(id).asInstanceOf[RArray]
        arr(index_eval).pop
        env
      //      TODO: thread reverse evaluation
      case AssignThread(id, bl) => env + (id -> bl)
      case RunThread(id) => eval_thread(env(id).asInstanceOf[Block], env)

      case If(_, bl1, bl2, if_res) =>
        val stack = env(if_res.id).asInstanceOf[RVar]
        if (stack.pop() == 1) {
          revEval(bl1, env + (if_res.id -> stack))
        } else {
          revEval(bl2, env + (if_res.id -> stack))
        }
      case While(b, bl, counter) =>
        val count = env(counter.id).asInstanceOf[RVar]
        if (count.top > 0) {
          revEval_stmt(While(b, bl, Counter(counter.id, counter.count-1)), revEval(bl, env))
        }
        else {
          count.pop
          env
        }
    }

  def revEval(bl: Block, env: Env): Env = bl match {
    case Nil => env
    case s :: bl => revEval(bl, revEval_stmt(s, env))
  }

}