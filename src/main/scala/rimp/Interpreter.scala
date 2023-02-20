package rimp

import scala.collection.mutable

class Interpreter extends Parser {

  // ------------ RIMP.Interpreter -------------------


  // an interpreter for the WHILE language
  type Env = Map[String, Any]


  def eval_aexp(a: AExp, env: Env): Int = a match {
    case Num(i) => i
    case Var(s) => env(s).asInstanceOf[mutable.Stack[Int]].top
    case ArrayVar(id, index) => {
      val valsList = env(id).asInstanceOf[Array[mutable.Stack[Int]]]
      val indexVal = eval_aexp(index, env)
      valsList(indexVal).top
    }
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
      case Assign(x, a) => {
        val old_stack = env.getOrElse(x, mutable.Stack[Int](0))
        env + (x -> old_stack.asInstanceOf[mutable.Stack[Int]].push(eval_aexp(a, env)))
      }
      case AssignArr(id, values) => {
        val old_array = env.getOrElse(id, Array.fill(values.length)(mutable.Stack[Int](0))).asInstanceOf[Array[mutable.Stack[Int]]]
        for ((arr, i) <- old_array.zipWithIndex) {
          arr.push(eval_aexp(values(i), env))
        }
        env + (id -> old_array)
      }
      case ArrayWithSize(id, size) => {
        val new_array = Array.fill(eval_aexp(size, env))(mutable.Stack[Int](0))
        env + (id -> new_array)
      }
      case UpdateArrIndex(id, index, newVal) => {
        val newVal_eval = eval_aexp(newVal, env)
        val index_eval = eval_aexp(index, env)
        val arr = env(id).asInstanceOf[Array[mutable.Stack[Int]]]
        arr(index_eval).push(newVal_eval)
        env
      }
      case AssignThread(id, bl) => env + (id -> bl)
      case RunThread(id) => eval_thread(env(id).asInstanceOf[Block], env)
      case If(b, bl1, bl2, if_res) => {
        val stack = env.getOrElse(if_res.id, mutable.Stack[Int](-1)).asInstanceOf[mutable.Stack[Int]]
        if (eval_bexp(b, env)) {
          stack.push(1) // represents true
          eval_bl(bl1, env + (if_res.id -> stack))
        } else {
          stack.push(0) // represents false
          eval_bl(bl2, env + (if_res.id -> stack))
        }
      }
      case While(b, bl, counter) => {
        if (eval_bexp(b, env)) {
          eval_stmt(While(b, bl, Counter(counter.id, counter.count+1)), eval_bl(bl, env + (counter.id -> (counter.count+1))))
        }
        else {
          env
        }
      }
    }

  def eval_bl(bl: Block, env: Env): Env = bl match {
    case Nil => env
    case s :: bl => eval_bl(bl, eval_stmt(s, env))
  }

  def eval(bl: Block, env: Env = Map()): Env = eval_bl(bl, env)

  def print_top(env: Env) = {
    env.map { case (key, value) =>
      value match {
        case i: Int => s"$key -> $i"
        case s: mutable.Stack[Int] => s"$key -> ${s.top}"
        case s_l: Array[mutable.Stack[Int] ] => {
          s"$key -> Array[${s_l.map(s => s.top).mkString(", ")}]"
        }
      }
    }.mkString("Map(", ", ", ")")
  }

}