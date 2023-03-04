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

  private def update_stack_top(c_stack: mutable.Stack[Int], i: Int): mutable.Stack[Int] = {
    c_stack.pop
    c_stack.push(i)
  }

  def eval_stmt(s: Stmt, env: Env): Env =
    s match {
      case Skip => env
      case Assign(x, a) =>
        val old_stack = env.getOrElse(x, stack())
        env + (x -> old_stack.asInstanceOf[RVar].push(eval_aexp(a, env)))
      case AssignArr(id, values) =>
        val old_stack = env.getOrElse(id, mutable.Stack(Array.fill(values.length)(stack()))).asInstanceOf[RArray]
        old_stack.push(values.map(v => stack(eval_aexp(v, env))))
        env + (id -> old_stack)
      case ArrayWithSize(id, size) =>
        val length = eval_aexp(size, env)
        val old_stack = env.getOrElse(id, mutable.Stack(Array.fill(length)(stack()))).asInstanceOf[RArray]
        old_stack.push(Array.fill(length)(stack()))
        env + (id -> old_stack)
      case UpdateArrIndex(id, index, newVal) =>
        val newVal_eval = eval_aexp(newVal, env)
        val index_eval = eval_aexp(index, env)
        val arr = env(id).asInstanceOf[RArray]
        arr.top(index_eval).push(newVal_eval)
        env
      case If(b, bl1, bl2, if_id) =>
        val stack = env.getOrElse(if_id, mutable.Stack[Int](0)).asInstanceOf[mutable.Stack[Int]]
        if (eval_bexp(b, env)) {
          stack.push(1) // represents true
          eval_bl(bl1, env + (if_id -> stack))
        } else {
          stack.push(0) // represents false
          eval_bl(bl2, env + (if_id -> stack))
        }
      case While(b, bl, counter) =>
        val c_stack = env.getOrElse(counter.id, mutable.Stack[Int](0)).asInstanceOf[mutable.Stack[Int]]
        if(counter.count == 0){
          c_stack.push(0)
        }
        if (eval_bexp(b, env)) {
          eval_stmt(While(b, bl, Counter(counter.id, counter.count+1)), eval_bl(bl, env + (counter.id -> update_stack_top(c_stack, counter.count+1))))
        }
        else {
          env
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
        case s: RVar => s"$key -> ${s.top.value}"
        case s: mutable.Stack[_] if s.head.isInstanceOf[Int] => s"$key -> ${s.head}"
        case s: mutable.Stack[_] if s.head.isInstanceOf[Array[RVar]] =>
          val top_arr = s.head.asInstanceOf[Array[RVar]]
          s"$key -> Array[${top_arr.map(e => e.top.value).mkString(", ")}]"
      }
    }.mkString("(", ", ", ")")
  }

  def revEval_stmt(s: Stmt, env: Env): Env =
    s match {
      case Skip => env
      case Assign(x, _) =>
        val old_stack = env(x).asInstanceOf[RVar]
        old_stack.pop
        env + (x -> old_stack)
      case AssignArr(id, _) =>
        val old_stack = env(id).asInstanceOf[RArray]
        old_stack.pop()
        env + (id -> old_stack)
      case ArrayWithSize(id, _) =>
        val old_stack = env(id).asInstanceOf[RArray]
        old_stack.pop()
        env + (id -> old_stack)
      case UpdateArrIndex(id, index, _) =>
        val index_eval = eval_aexp(index, env)
        val arr = env(id).asInstanceOf[RArray].top
        arr(index_eval).pop
        env
      case If(_, bl1, bl2, if_id) =>
        val stack = env(if_id).asInstanceOf[mutable.Stack[Int]]
        if (stack.pop == 1) {
          revEval(bl1, env + (if_id -> stack))
        } else {
          revEval(bl2, env + (if_id -> stack))
        }
      case While(b, bl, counter) =>
        val c_stack = env(counter.id).asInstanceOf[mutable.Stack[Int]]
        val c_top = c_stack.top
        if (c_top > 0) {
          revEval_stmt(While(b, bl, counter), revEval(bl, env + (counter.id -> update_stack_top(c_stack, c_top-1))))
        } else c_stack.pop
        env
    }

  def revEval(bl: Block, env: Env): Env = bl match {
    case Nil => env
    case s :: bl => revEval(bl, revEval_stmt(s, env))
  }

}