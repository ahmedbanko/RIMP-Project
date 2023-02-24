package rimp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class InterpreterTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach {

  val interp = new Interpreter
  val fixtures = new Fixtures

  type Env = Map[String, Any]
  var env: Env = Map()

  override protected def afterEach(): Unit = {
    super.afterEach()
    env = Map() // clear environment
  }

  var while_count: Int = -1
  var if_count: Int = -1

  def whileID(): String = {
    while_count += 1
    s"_k$while_count"
  }

  def ifID(): String = {
    if_count += 1
    s"_if$if_count"
  }



  test("Test evaluation of arithmatic expressions") {
    assert(interp.eval_aexp(interp.Num(1), env) == 1)
    assertThrows[java.util.NoSuchElementException](interp.eval_aexp(interp.Var("x"), env))
    env = env + ("x" -> interp.stack(10))
    assert(interp.eval_aexp(interp.Var("x"), env) == 10)
    assertThrows[java.util.NoSuchElementException](interp.eval_aexp(interp.ArrayVar("arr", interp.Num(0)), env) == 1)
    env = env + ("arr" -> Array(interp.stack(0,1),interp.stack(0,2),interp.stack(0,3)))
    assert(interp.eval_aexp(interp.ArrayVar("arr", interp.Num(0)), env) == 1)
    assert(interp.eval_aexp(interp.Aop("+", interp.Num(1), interp.Num(10)), env) == 11)
    assert(interp.eval_aexp(interp.Aop("-", interp.Num(10), interp.Num(5)), env) == 5)
    assert(interp.eval_aexp(interp.Aop("*", interp.Num(10), interp.Num(10)), env) == 100)
    assert(interp.eval_aexp(interp.Aop("/", interp.Num(10), interp.Num(10)), env) == 1)
    assert(interp.eval_aexp(interp.Aop("%", interp.Num(10), interp.Num(2)), env) == 0)
  }


  test("Test evaluation of boolean expressions") {
    assert(interp.eval_bexp(interp.True, env))
    assert(!interp.eval_bexp(interp.False, env))
    assert(interp.eval_bexp(interp.Bop("=", interp.Num(10), interp.Num(10)), env))
    assert(!interp.eval_bexp(interp.Bop("=", interp.Num(0), interp.Num(10)), env))
    assert(!interp.eval_bexp(interp.Bop("!=", interp.Num(10), interp.Num(10)), env))
    assert(interp.eval_bexp(interp.Bop("!=", interp.Num(0), interp.Num(10)), env))
    assert(interp.eval_bexp(interp.Bop(">", interp.Num(10), interp.Num(9)), env))
    assert(!interp.eval_bexp(interp.Bop(">", interp.Num(0), interp.Num(10)), env))
    assert(interp.eval_bexp(interp.Bop("<", interp.Num(10), interp.Num(11)), env))
    assert(!interp.eval_bexp(interp.Bop("<", interp.Num(10), interp.Num(10)), env))
    assert(interp.eval_bexp(interp.Bop("<=", interp.Num(10), interp.Num(10)), env))
    assert(interp.eval_bexp(interp.Bop("<=", interp.Num(10), interp.Num(11)), env))
    assert(!interp.eval_bexp(interp.Bop("<=", interp.Num(10), interp.Num(9)), env))
    assert(interp.eval_bexp(interp.Bop(">=", interp.Num(10), interp.Num(10)), env))
    assert(interp.eval_bexp(interp.Bop(">=", interp.Num(11), interp.Num(10)), env))
    assert(!interp.eval_bexp(interp.Bop(">=", interp.Num(9), interp.Num(99)), env))
    assert(!interp.eval_bexp(interp.Not(interp. Bop("=", interp.Num(10), interp.Num(10))), env))
  }


  test("Test evaluation of statements") {
    assert(interp.eval_stmt(interp.Skip, env) == env)
    assertThrows[java.util.NoSuchElementException](env("x") == 10)
    env = interp.eval_stmt(interp.Assign("x", interp.Num(10)), env)
    assert(env("x").asInstanceOf[interp.RVar].top == 10)
    assertThrows[java.util.NoSuchElementException](env("arr") == Array(interp.stack(0,1),interp.stack(0,2),interp.stack(0,3)))
    env = interp.eval_stmt(interp.AssignArr("arr", Array(interp.Num(10), interp.Num(9), interp.Num(8))), env)
    assert(env("arr").asInstanceOf[interp.RArray] sameElements Array(interp.stack(10),interp.stack(9),interp.stack(8)))
    val arr = env("arr").asInstanceOf[interp.RArray]
    assert(arr.head.top == 10)
    env = interp.eval_stmt(interp.UpdateArrIndex("arr", interp.Num(0),interp.Num(99)), env)
    val arr2 = env("arr").asInstanceOf[interp.RArray]
    assert(arr2.head.top != 10 && arr2.head.top == 99)
    env = Map() // clear environment
    assert(interp.eval_stmt(interp.If(interp.True, List(interp.Skip), List(interp.Assign("i", interp.Num(10))), interp.IfResult("id", interp.stack())), env) == env + ("id" -> interp.stack(  1)))
    env = interp.eval_stmt(interp.If(interp.False, List(interp.Skip), List(interp.Assign("i", interp.Num(1))), interp.IfResult("id", interp.stack())), env)
    assert(env("id").asInstanceOf[interp.RVar] == interp.stack(0))
    assert(env("i").asInstanceOf[interp.RVar].top == 1)
    val w_id = whileID()
    assert(interp.eval_stmt(interp.While(interp.False, List(interp.Skip), interp.Counter(w_id)), env) == env + (w_id -> interp.stack( 0)))
    env = interp.eval_stmt(interp.While(interp.Bop(">", interp.Var("i"), interp.Num(0)), List(interp.Assign("i", interp.Num(0))), interp.Counter(whileID())), env)
    assert(env("i").asInstanceOf[interp.RVar].top == 0)
    env = Map() // clear environment
    env = interp.eval_stmt(interp.ArrayWithSize("arr",interp.Num(10)), env)
    assert(env("arr").asInstanceOf[interp.RArray].length == 10)
    assert(env("arr").asInstanceOf[interp.RArray](0).top == 0)
    assert(env("arr").asInstanceOf[interp.RArray](9).top == 0)
    assertThrows[java.lang.ArrayIndexOutOfBoundsException](env("arr").asInstanceOf[interp.RArray](-1).top == 0)
    assertThrows[java.lang.ArrayIndexOutOfBoundsException](env("arr").asInstanceOf[interp.RArray](10).top == 0)
    env = interp.eval(interp.parse("arr := [1,2,3,4,5,6,7,8,9,10]"), env)
    env = interp.eval(interp.parse("arr[0] := 2"), env)
    env = interp.eval(interp.parse("arr[1] := 3"), env)
    val stack_arr = env("arr").asInstanceOf[interp.RArray]
    assert(stack_arr(0).length == 3)
    assert(stack_arr(0) == interp.stack(1, 2))
    assert(stack_arr(1).length == 3)
    assert(stack_arr(1) == interp.stack(2, 3))
    assert(stack_arr(2).length == 2)
    assert(stack_arr(9).length == 2)
  }


  test("Test revEval_stmt of AssignArr") {
    val ast = interp.parse("arr := [1,2,3,4,5,6,7,8,9,10];arr := [0,0,0,0,0,0,0,0,0,0];arr := [1,2,3,4,5,6,7,8,9,10]")
    env = interp.eval(ast, env)
    val stack_arr = env("arr").asInstanceOf[interp.RArray]
    for (i <- stack_arr.indices) {
      assert(stack_arr(i).length == 4)
    }
    env = interp.revEval(interp.revAST(ast), env)
    val rev_stack_arr = env("arr").asInstanceOf[interp.RArray]
    for (i <- rev_stack_arr.indices) {
      assert(rev_stack_arr(i).length == 1)
      assert(rev_stack_arr(i).top == 0)
    }
  }


  test("Test revEval_stmt of ArrWithSize") {
    val ast = interp.parse("arr := |10|;arr := [1,2,3,4,5,6,7,8,9,10];arr := [0,0,0,0,0,0,0,0,0,0];arr := [1,2,3,4,5,6,7,8,9,10]")
    env = interp.eval(ast, env)
    val stack_arr = env("arr").asInstanceOf[interp.RArray]
    for (i <- stack_arr.indices) {
      assert(stack_arr(i).length == 4)
      assert(stack_arr(i).top == i+1)
    }
    env = interp.revEval(interp.revAST(ast), env)
    val rev_stack_arr = env("arr").asInstanceOf[interp.RArray]
    for (i <- rev_stack_arr.indices) {
      assert(rev_stack_arr(i).length == 1)
      assert(rev_stack_arr(i).top == 0)
    }
  }


  test("Test revEval_stmt of UpdateArrIndex") {
    val ast = interp.parse("arr := |10|;arr := [1,2,3,4,5,6,7,8,9,10];arr[0]:= 11")
    env = interp.eval(ast, env)
    val stack_arr = env("arr").asInstanceOf[interp.RArray]
    assert(stack_arr(0).length == 3)
    assert(stack_arr(0).top == 11)
    for (i <- 1 until stack_arr.length) {
      assert(stack_arr(i).length == 2)
      assert(stack_arr(i).top == i + 1)
    }
    env = interp.revEval(interp.revAST(ast), env)
    val rev_stack_arr = env("arr").asInstanceOf[interp.RArray]

    for (i <- rev_stack_arr.indices) {
      assert(rev_stack_arr(i).length == 1)
      assert(rev_stack_arr(i).top == 0)
    }
  }

  test("Test revEval_stmt of If-then-else when if true") {
    val ast = interp.parse(fixtures.if_true_prog)
    val my_if = ast.filter(x => x.isInstanceOf[interp.If]).head
    val my_if_id = my_if.asInstanceOf[interp.If].boolStack.id
    env = interp.eval(ast, env)
    assert(env("x").asInstanceOf[interp.RVar].length == 3)
    assert(env("x").asInstanceOf[interp.RVar].top == 9)
    assert(env("y").asInstanceOf[interp.RVar].length == 2)
    assert(env("y").asInstanceOf[interp.RVar].top == 100)
    assert(env(my_if_id).asInstanceOf[interp.RVar].length == 2)
    assert(env(my_if_id).asInstanceOf[interp.RVar] == interp.stack(1))

    env = interp.revEval(interp.revAST(ast), env)
    assert(env("x").asInstanceOf[interp.RVar].length == 1)
    assert(env("x").asInstanceOf[interp.RVar].top == 0)
    assert(env("y").asInstanceOf[interp.RVar].length == 1)
    assert(env("y").asInstanceOf[interp.RVar].top == 0)
    assert(env(my_if_id).asInstanceOf[interp.RVar].length == 1)
    assert(env(my_if_id).asInstanceOf[interp.RVar] == interp.stack())
  }


  test("Test revEval_stmt of If-then-else when if false") {
    val ast = interp.parse(fixtures.if_false_prog)
    val my_if = ast.filter(x => x.isInstanceOf[interp.If]).head
    val my_if_id = my_if.asInstanceOf[interp.If].boolStack.id
    env = interp.eval(ast, env)
    assert(env("x").asInstanceOf[interp.RVar].length == 2)
    assert(env("x").asInstanceOf[interp.RVar].top == 10)
    assert(env("y").asInstanceOf[interp.RVar].length == 3)
    assert(env("y").asInstanceOf[interp.RVar].top == 99)
    assert(env(my_if_id).asInstanceOf[interp.RVar].length == 2)
    assert(env(my_if_id).asInstanceOf[interp.RVar] == interp.stack(0))

    env = interp.revEval(interp.revAST(ast), env)
    assert(env("x").asInstanceOf[interp.RVar].length == 1)
    assert(env("x").asInstanceOf[interp.RVar].top == 0)
    assert(env("y").asInstanceOf[interp.RVar].length == 1)
    assert(env("y").asInstanceOf[interp.RVar].top == 0)
    assert(env(my_if_id).asInstanceOf[interp.RVar].length == 1)
    assert(env(my_if_id).asInstanceOf[interp.RVar] == interp.stack())
  }


  test("Test evaluation of blocks") {
    assert(interp.eval_bl(List(), env) == env)
    env = interp.eval_bl(List(interp.Assign("i10", interp.Num(10)), interp.Assign("i11", interp.Num(11))), env)
    assert(env("i10").asInstanceOf[interp.RVar] == interp.stack(10))
    assert(env("i10").asInstanceOf[interp.RVar].top == 10)
    assert(env("i11").asInstanceOf[interp.RVar] == interp.stack(11))
    assert(env("i11").asInstanceOf[interp.RVar].top == 11)
  }

  test("Test eval function") {
    assert(interp.eval(List()) == env)
    env = interp.eval(List(interp.Assign("i10", interp.Num(10)), interp.Assign("i11", interp.Num(11))))
    assert(env("i10").asInstanceOf[interp.RVar] == interp.stack(10))
    assert(env("i10").asInstanceOf[interp.RVar].top == 10)
    assert(env("i11").asInstanceOf[interp.RVar] == interp.stack(11))
    assert(env("i11").asInstanceOf[interp.RVar].top == 11)
  }




  test("Test creating empty array with size should have a correct size") {
    env = interp.eval(interp.parse("arr := |10|"))
    assert(env("arr").asInstanceOf[interp.RArray].length == 10)
  }

  test("Assigning array indexes should work correctly") {
    env = interp.eval(interp.parse(fixtures.arrProg1))
    assert(env("i").asInstanceOf[interp.RVar].top == 10)
    assert(env("ii").asInstanceOf[interp.RVar].top == 10)
    assert(env("x").asInstanceOf[interp.RVar].top == 9)
    val arr = env("arr").asInstanceOf[interp.RArray]
    for(i <- 0  until 10){
      assert(arr(i).top == i)
    }
  }

  test("Test creating an array with values") {
    env = interp.eval(interp.parse("arr := [1, 2, 3]"))
    assert(env("arr").asInstanceOf[interp.RArray].length == 3)
    val arr = env("arr").asInstanceOf[interp.RArray]
    assert(arr(0).top == 1)
    assert(arr(0)== interp.stack(1))
    assert(arr(1).top == 2)
    assert(arr(1)== interp.stack(2))
    assert(arr(2).top == 3)
    assert(arr(2)== interp.stack(3))
  }

  test("Test getting value from array indexes") {
    env = interp.eval(interp.parse("arr := [1, 2, 3]; i0 := arr[0]; i1 := arr[1]; i2 := arr[2]"))
    assert(env("arr").asInstanceOf[interp.RArray].length == 3)
    assert(env("i0").asInstanceOf[interp.RVar].top == 1)
    assert(env("i1").asInstanceOf[interp.RVar].top  == 2)
    assert(env("i2").asInstanceOf[interp.RVar].top  == 3)
    assertThrows[java.lang.ArrayIndexOutOfBoundsException](interp.eval(interp.parse("i3 := arr[3]"), env))
    assertThrows[java.lang.ArrayIndexOutOfBoundsException](interp.eval(interp.parse("i := arr[-1]"), env))
  }


  test("Test stack_tops function") {
    val ast = interp.parse(fixtures.EX1)
    val my_while = ast.filter(x => x.isInstanceOf[interp.While]).head
    val my_while_id = my_while.asInstanceOf[interp.While].counter.id
    env = interp.eval(ast)
    env = interp.eval(interp.parse("arr := [1,2,3]"), env)

    assert(interp.stack_tops(env) == s"Map(fact -> 6, n -> 0, ${my_while_id} -> 3, arr -> Array[1, 2, 3])")
  }


  test("Test revEval function") {
    val ast = interp.parse(fixtures.EX1)
    val my_while = ast.filter(x => x.isInstanceOf[interp.While]).head
    val my_while_id = my_while.asInstanceOf[interp.While].counter.id
    env = interp.eval(ast)
    env = interp.revEval(interp.revAST(ast), env)
    assert(interp.stack_tops(env) == s"Map(fact -> 0, n -> 0, ${my_while_id} -> 0)")
    val fact_stack = env("fact").asInstanceOf[interp.RVar]
    assert(fact_stack.length == 1)
    val n_stack = env("n").asInstanceOf[interp.RVar]
    assert(n_stack.length == 1)
  }

  test("Test forward evaluation of pre-defined program 1") {
    val ast = interp.parse(fixtures.EX1)
    val env = interp.eval(ast)
    assert(env("fact").asInstanceOf[interp.RVar] == interp.stack(1, 3, 6, 6))
    assert(env("n").asInstanceOf[interp.RVar] == interp.stack(3, 2, 1, 0))
    val w = ast.filter(x => x.isInstanceOf[interp.While]).head
    val w_id = w.asInstanceOf[interp.While].counter.id

    assert(env(w_id).asInstanceOf[interp.RVar] == interp.stack(3))
  }



  test("Test forward evaluation of pre-defined program 2") {
    val ast = interp.parse(fixtures.EX2)
    val env = interp.eval(ast)
    assert(env("a").asInstanceOf[interp.RVar] == interp.stack(49, 21, 14, 7))
    assert(env("b").asInstanceOf[interp.RVar] == interp.stack(28, 7))
    assert(env("_if6").asInstanceOf[interp.RVar] == interp.stack(1, 0, 1, 1))
    assert(env("_k12").asInstanceOf[interp.RVar] == interp.stack(4))
  }

  test("Test forward evaluation of pre-defined program 3") {
    val ast = interp.parse(fixtures.EX3)
    val env = interp.eval(ast)
    assert(env("x").asInstanceOf[interp.RVar] == interp.stack(12,6,3,10,5,16,8,4,2,1))
    assert(env("r").asInstanceOf[interp.RVar] == interp.stack(12, 10, 8, 6, 4, 2, 0, 6, 4, 2, 0, 3, 1, 10, 8, 6, 4, 2, 0, 5, 3, 1, 16, 14, 12, 10, 8, 6, 4, 2, 0, 8, 6, 4, 2, 0, 4, 2, 0, 2, 0))
    assert(env("_if17").asInstanceOf[interp.RVar] == interp.stack( 1, 1, 0, 1, 0, 1, 1, 1, 1))
    assert(env("_k17").asInstanceOf[interp.RVar] == interp.stack( 6, 3, 1, 5, 2, 8, 4, 2, 1))
    assert(env("_k19").asInstanceOf[interp.RVar] == interp.stack( 9))
  }

  test("Test forward evaluation of pre-defined program 4") {
    val ast = interp.parse(fixtures.EX4)
    val env = interp.eval(ast)
    assert(env("x").asInstanceOf[interp.RVar] == interp.stack(13))
    assert(env("r").asInstanceOf[interp.RVar] == interp.stack(13, 11, 9, 7, 5, 3, 1, 13, 10, 7, 4, 1, 13, 9, 5, 1, 13, 8, 3, 13, 7, 1))
    assert(env("isprime").asInstanceOf[interp.RVar] == interp.stack(1))
    assert(env("limit").asInstanceOf[interp.RVar] == interp.stack(7))
    assert(env("factor").asInstanceOf[interp.RVar] == interp.stack(2, 3, 4, 5, 6, 7))
    assert(env("_k29").asInstanceOf[interp.RVar] == interp.stack(5))
    assert(env("_k25").asInstanceOf[interp.RVar] == interp.stack(6, 4, 3, 2, 2))
    assert(env("_if22").asInstanceOf[interp.RVar] == interp.stack(0,0,0,0,0))
  }

  test("Test forward evaluation of pre-defined program 5") {
    val ast = interp.parse(fixtures.EX5)
    val env = interp.eval(ast)
    assert(env("a").asInstanceOf[interp.RVar] == interp.stack(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89))
    assert(env("b").asInstanceOf[interp.RVar] == interp.stack(1,2,3,5,8,13,21,34,55,89,144))
    assert(env("i").asInstanceOf[interp.RVar] == interp.stack(0,1,2,3,4,5,6,7,8,9,10))
    assert(env("tmp").asInstanceOf[interp.RVar] == interp.stack(1,1,2,3,5,8,13,21,34,55))
    assert(env("_k31").asInstanceOf[interp.RVar] == interp.stack(10))
  }

  test("Test backward evaluation of pre-defined programs") {
    for(p <- fixtures.allExamples){
      // Forward evaluation
      val ast = interp.parse(p)
      val env = interp.eval(ast)

      // Backward evaluation
      val rev_ast = interp.revAST(ast)
      val rev_env = interp.revEval(rev_ast, env)
      rev_env.values.foreach(s => assert(stackOnlyHasZero(s)))
    }
  }


  def stackOnlyHasZero(stack: Any): Boolean = stack match {
    case s: interp.RVar => s.length == 1 && s.top == 0
    case s: interp.RArray => s.forall(i => i.length == 1 && i.top == 0)
    case _ => throw new RuntimeException("stackOnlyHasZero function can only check RVar and RArray types")
  }
  
}
