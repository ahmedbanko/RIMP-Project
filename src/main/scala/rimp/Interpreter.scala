package rimp

class Interpreter extends Parser {

  // ------------ RIMP.Interpreter -------------------


  // an interpreter for the WHILE language
  type Env = Map[String, Any]

//  def strList2IntArray(in: String): Array[Int] = {
//    val out = in.stripPrefix("Array(").stripSuffix(")").split(", ").map(_.toInt).toArray
//    print(out)
//    out
//  }


  def eval_aexp(a: AExp, env: Env): Int = a match {
    case Num(i) => i
    case Var(s) => env(s).asInstanceOf[Int]
    case ArrayVar(id, index) => {

      val valsList = env(id).asInstanceOf[Array[Int]]
//      val intList = strList2IntArray(valsList)
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

  def eval_arrVals(values: Array[AExp], env: Env): Array[Int] =
    values.map(x => eval_aexp(x, env))



// TODO: Make sure the thread works correctly

  def eval_thread(bl: Block, env: Env): Env = {
    @volatile var e: Env = Map()
    this.synchronized {
      e = e ++ env
      val thread = new Thread {
        override def run {
          e = e ++ eval_bl(bl, env)
        }
      }
      thread.start()
    }
    e
  }

  def eval_stmt(s: Stmt, env: Env): Env =
    s match {
      case Skip => env
      case Assign(x, a) => env + (x -> eval_aexp(a, env))
      case AssignArr(id, values) => env + (id -> eval_arrVals(values, env))
      case UpdateArrIndex(id, index, newVal) => {
        val newVal_eval = eval_aexp(newVal, env)
        val index_eval = eval_aexp(index, env)
        env + (id -> env(id).asInstanceOf[Array[Int]].updated(index_eval, newVal_eval))
      }
      case WriteVar(x) =>
        println(env(x));
        env
      case WriteStr(x) =>
        //Expands standard Scala escape sequences in a string. copied from:
        //https://www.scala-lang.org/api/2.13.6/scala/StringContext$.html
        print(StringContext.processEscapes(x.substring(1, x.length - 1)))
        env
      case AssignThread(id, bl) => env + (id -> bl)
      case RunThread(id) => eval_thread(env(id).asInstanceOf[Block], env)
      case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env)
      case While(b, bl) =>
        if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
        else env
    }

  def eval_bl(bl: Block, env: Env): Env = bl match {
    case Nil => env
    case s :: bl => eval_bl(bl, eval_stmt(s, env))
  }

  def eval(bl: Block): Env = eval_bl(bl, Map())

}
