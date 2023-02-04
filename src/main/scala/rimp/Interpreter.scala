package rimp

class Interpreter extends Parser {

  // ------------ RIMP.Interpreter -------------------


  // an interpreter for the WHILE language
  type Env = Map[String, Any]

  def strList2IntList(in: String): List[Int] =
    in.stripPrefix("List(").stripSuffix(")").split(", ").map(_.toInt).toList


  def eval_aexp(a: AExp, env: Env): Int = a match {
    case Num(i) => i
    case Var(s) => env(s).asInstanceOf[Int]
    case ArrayVar(id, index) => {
      val valsList = env(id).toString
      val intList = strList2IntList(valsList)
      val indexVal = eval_aexp(index, env)
      intList(indexVal)
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

  def eval_arrVals(values: List[AExp], env: Env): List[Int] =
    values.map(x => eval_aexp(x, env))

  def eval_stmt(s: Stmt, env: Env): Env =
    s match {
      case Skip => env
      case Assign(x, a) => env + (x -> eval_aexp(a, env))
      case AssignArr(id, values) => env + (id -> eval_arrVals(values, env))
      case UpdateArrIndex(id, index, newVal) => {
        val newVal_eval = eval_aexp(newVal, env)
        val index_eval = eval_aexp(index, env)
        env + (id -> strList2IntList(env(id).toString).updated(index_eval, newVal_eval))
      }
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