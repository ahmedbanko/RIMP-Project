package rimp

import scala.collection.mutable

class Interpreter extends Parser {

  // ------------ RIMP.Interpreter -------------------



  val controlStack = new mutable.Stack()
  val resultStack = new mutable.Stack()
  val memoryStack = new mutable.Stack[Env]() // where intermediate results of computations are stored
  val backStack = new mutable.Stack()

  val configs = (controlStack, resultStack, memoryStack, backStack)

   // abstract machine for RIMP are tuples <control, result, memory, back>
   // Initial configurations have the form <P · nil, nil, m, nil>
   // reversed abstract machine for RIMP are tuples <back, result, memory, control>


  type Env = Map[String, Any]
//  var currentEnv = Map[String, Any]()

  val stmtStack = new mutable.Stack[Stmt]()


  def eval_aexp(a: AExp, env: Env): Int = a match {
    case Num(i) => i
    case Var(s) => {
      val (result, stmt) = env(s)
      result.asInstanceOf[Int]
    }
    case ArrayVar(id, index) => {
      val (vals, stmt) = env(id)
      val valsArray = vals.asInstanceOf[Array[Int]]
      val indexVal = eval_aexp(index, env)
      valsArray(indexVal)
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
      case Skip => {
        println(s"Statement $s -> $env")
        memoryStack.push(env)
        env
      }
      case Assign(x, a) => {
        val e  =  env + (x -> (eval_aexp(a, env), Assign(x, a)))
        println(s"Statement $s -> $e")
          memoryStack.push(e)
        e
      }
      case AssignArr(id, values) => {
        val e  = env + (id -> (values.map(x => eval_aexp(x, env)), AssignArr(id, values)))
        println(s"Statement $s -> $e")
        memoryStack.push(e)
        e
      }
      case ArrayWithSize(id, size) => {
        val e = env + (id -> (new Array[Int](eval_aexp(size, env)), ArrayWithSize(id, size)))
        println(s"Statement $s -> $e")
        memoryStack.push(e)
        e
      }
      case UpdateArrIndex(id, index, newVal) => {
        val newVal_eval = eval_aexp(newVal, env)
        val index_eval = eval_aexp(index, env)
        val (result, stmt) = env(id)
        val e = env + (id -> (result.asInstanceOf[Array[Int]].updated(index_eval, newVal_eval), UpdateArrIndex(id, index, newVal)))
        println(s"Statement $s -> $e")
        memoryStack.push(e)
        e
      }
      case AssignThread(id, bl) => {
        val e  = env + (id -> (bl, AssignThread(id, bl)))
        println(s"Statement $s -> $e")
        memoryStack.push(e)
        e
      }
      case RunThread(id) => {
        val (result, stmt) = env(id)
        val e = eval_thread(result.asInstanceOf[Block], env)
        println(s"Statement $s -> $e")
        memoryStack.push(e)
        e
      }
      case If(b, bl1, bl2) => if (eval_bexp(b, env)) {
        val e = eval_bl(bl1, env)
        println(s"Statement $s -> $e")
        memoryStack.push(e)
        e
      } else {
        val e = eval_bl(bl2, env)
        println(s"Statement $s -> $e")
        memoryStack.push(e)
        e
      }
      case While(b, bl, counter) =>
        if (eval_bexp(b, env)) {
          val e = eval_stmt(While(b, bl, counter+1), eval_bl(bl, env))
          println(s"Statement $s -> $e")
          memoryStack.push(e)
          e
        }
        else {
          println(s"Statement $s -> $env")
          memoryStack.push(env)
          env
        }
    }

  def eval_bl(bl: Block, env: Env): Env = bl match {
    case Nil => env
    case s :: bl => {
      stmtStack.push(s)
      eval_bl(bl, eval_stmt(s, env))
    }
  }

  def eval(bl: Block, env: Env = Map()): Env = eval_bl(bl, env)



//
//  def revEval(bl: Block, env: Env): Env = {
//    val lastEnv = envStack.pop()
//    stmtStack.toList match {
//      case Nil => env
//      case s :: bl => revEval()
//
//    }
//  }
//  def revStmtEval(s: Stmt, env: Env): Env = {
//      val lastEnv = envStack.pop()
//      s match {
//        case Skip => env
//        case ArrayWithSize(id, size) => env + (id -> lastEnv(id))
//        case Assign(s, a) => env + (s -> lastEnv(s))
//        case AssignArr(id, values) => env + (id -> lastEnv(id))
//        case AssignThread(id, bl) =>  env + (id -> lastEnv(id))
//        case If(a, bl1, bl2) => env
//        case RunThread(id) =>  env + (id -> lastEnv(id))
//        case UpdateArrIndex(id, index, newVal) =>  env + (id -> lastEnv(id))
//        case While(b, bl, counter) => env
//      }
//  }

//  def revEval(s: Stmt) = s match {
//    case Num(n) =>
//  }

}
