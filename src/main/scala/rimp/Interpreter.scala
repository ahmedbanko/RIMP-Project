package rimp

import scala.collection.mutable

class Interpreter extends Parser {

  // ------------ RIMP.Interpreter -------------------

//  case class Mem(env: Map[String, Any], bStack: mutable.Stack[(String, Stmt)])

//  val backStack = new mutable.Stack[(String, Any, Stmt)]()

  // an interpreter for the WHILE language
  type Env = (Map[String, Any], mutable.Stack[(String, Stmt)])


  def eval_aexp(a: AExp, env: Env): Int = a match {
    case Num(i) => i
    case Var(s) => {
      val (e, b) = env
      e(s).asInstanceOf[Int]
    }
    case ArrayVar(id, index) => {
      val (e, b) = env
      val valsList = e(id).asInstanceOf[Array[Int]]
      val indexVal = eval_aexp(index, env)
      valsList(indexVal)
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


  def eval_thread(bl: Block, env: Env, bStack: mutable.Stack[(String, Stmt)]): Env = {
    new Thread(() => {
      eval_bl(bl, env, bStack)
    }).start()
    env
  }

  def eval_stmt(s: Stmt, env: Env, bStack: mutable.Stack[(String, Stmt)]): Env =
    s match {
      case Skip => {
        bStack.push(("", Skip))
        env
      }
      case Assign(x, a) => {
        val assign_res = eval_aexp(a, env)
        bStack.push((x, Assign(x, a)))
        val e = env._1 + (x -> assign_res)
        (e, bStack)
      }
//      case AssignArr(id, values) => env + (id -> values.map(x => eval_aexp(x, env)))
      case ArrayWithSize(id, size) => {
        bStack.push((id, ArrayWithSize(id, size)))
        val assign_res = new Array[Int](eval_aexp(size, env))
        val e = env._1 + (id -> assign_res)
        (e, bStack)
      }
//      case UpdateArrIndex(id, index, newVal) => {
//        val newVal_eval = eval_aexp(newVal, env)
//        val index_eval = eval_aexp(index, env)
//        env + (id -> env(id).asInstanceOf[Array[Int]].updated(index_eval, newVal_eval))
//      }
//      case WriteVar(x) =>
//        println(env(x));
//        env
//      case WriteStr(x) =>
//        //Expands standard Scala escape sequences in a string. copied from:
//        //https://www.scala-lang.org/api/2.13.6/scala/StringContext$.html
//        print(StringContext.processEscapes(x.substring(1, x.length - 1)))
//        env
//      case AssignThread(id, bl) => env + (id -> bl)
//      case RunThread(id) => eval_thread(env(id).asInstanceOf[Block], env, bStack)
//      case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env, bStack) else eval_bl(bl2, env, bStack)
//      case While(b, bl) =>
//        if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env, bStack), bStack)
//        else env
    }

  def eval_bl(bl: Block, env: Env, bStack: mutable.Stack[(String, Stmt)]): Env = bl match {
    case Nil => env
    case s :: bl => eval_bl(bl, eval_stmt(s, env, bStack), bStack)
  }

  def eval(bl: Block, env: Env = (Map().empty, new mutable.Stack[(String, Stmt)]), bStack: mutable.Stack[(String, Stmt)] = mutable.Stack()): Env = eval_bl(bl, env, bStack)

  def revEval(env: Env) : Env = {
    val backBlock: Block = env._2.map(e => e._2).toList
   eval(backBlock, (Map().empty, new mutable.Stack))
  }
}
