package rimp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class InterpreterTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach {

  val i = new Interpreter
  val fixtures = new Fixtures

  type Env = Map[String, Any]
  var env: Env = Map()

  override protected def afterEach(): Unit = {
    super.afterEach()
    // clear environment
    env = Map()
    // reset counters
    i.while_count = 0
    i.if_count = 0
  }


  test("Test evaluation of arithmatic expressions") {
    assert(i.eval_aexp(i.Num(1), env) == 1)
    assertThrows[java.util.NoSuchElementException](i.eval_aexp(i.Var("x"), env))
    env = env + ("x" -> i.stack(0, 10))
    assert(i.eval_aexp(i.Var("x"), env) == 10)
    assertThrows[java.util.NoSuchElementException](i.eval_aexp(i.ArrayVar("arr", i.Num(0)), env) == 1)
    env = env + ("arr" -> Array(i.stack(0, 0,1),i.stack(0, 0,2),i.stack(0, 0,3)))
    assert(i.eval_aexp(i.ArrayVar("arr", i.Num(0)), env) == 1)
    assert(i.eval_aexp(i.Aop("+", i.Num(1), i.Num(10)), env) == 11)
    assert(i.eval_aexp(i.Aop("-", i.Num(10), i.Num(5)), env) == 5)
    assert(i.eval_aexp(i.Aop("*", i.Num(10), i.Num(10)), env) == 100)
    assert(i.eval_aexp(i.Aop("/", i.Num(10), i.Num(10)), env) == 1)
    assert(i.eval_aexp(i.Aop("%", i.Num(10), i.Num(2)), env) == 0)
  }


  test("Test evaluation of boolean expressions") {
    assert(i.eval_bexp(i.True, env))
    assert(!i.eval_bexp(i.False, env))
    assert(i.eval_bexp(i.Bop("=", i.Num(10), i.Num(10)), env))
    assert(!i.eval_bexp(i.Bop("=", i.Num(0), i.Num(10)), env))
    assert(!i.eval_bexp(i.Bop("!=", i.Num(10), i.Num(10)), env))
    assert(i.eval_bexp(i.Bop("!=", i.Num(0), i.Num(10)), env))
    assert(i.eval_bexp(i.Bop(">", i.Num(10), i.Num(9)), env))
    assert(!i.eval_bexp(i.Bop(">", i.Num(0), i.Num(10)), env))
    assert(i.eval_bexp(i.Bop("<", i.Num(10), i.Num(11)), env))
    assert(!i.eval_bexp(i.Bop("<", i.Num(10), i.Num(10)), env))
    assert(i.eval_bexp(i.Bop("<=", i.Num(10), i.Num(10)), env))
    assert(i.eval_bexp(i.Bop("<=", i.Num(10), i.Num(11)), env))
    assert(!i.eval_bexp(i.Bop("<=", i.Num(10), i.Num(9)), env))
    assert(i.eval_bexp(i.Bop(">=", i.Num(10), i.Num(10)), env))
    assert(i.eval_bexp(i.Bop(">=", i.Num(11), i.Num(10)), env))
    assert(!i.eval_bexp(i.Bop(">=", i.Num(9), i.Num(99)), env))
    assert(!i.eval_bexp(i.Not(i. Bop("=", i.Num(10), i.Num(10))), env))
  }


  test("Test evaluation of statements") {
    assert(i.eval_stmt(i.Skip, env) == env)
    assertThrows[java.util.NoSuchElementException](env("x") == 10)
    env = i.eval_stmt(i.Assign("x", i.Num(10)), env)
    assert(env("x").asInstanceOf[i.RVar].top == 10)
    assertThrows[java.util.NoSuchElementException](env("arr") == Array(i.stack(0, 0,1),i.stack(0, 0,2),i.stack(0, 0,3)))
    env = i.eval_stmt(i.AssignArr("arr", Array(i.Num(10), i.Num(9), i.Num(8))), env)
    assert(env("arr").asInstanceOf[i.RArray] sameElements Array(i.stack(0, 10),i.stack(0, 9),i.stack(0, 8)))
    val arr = env("arr").asInstanceOf[i.RArray]
    assert(arr.head.top == 10)
    env = i.eval_stmt(i.UpdateArrIndex("arr", i.Num(0),i.Num(99)), env)
    val arr2 = env("arr").asInstanceOf[i.RArray]
    assert(arr2.head.top != 10 && arr2.head.top == 99)
    env = Map() // clear environment
    assert(i.eval_stmt(i.If(i.True, List(i.Skip), List(i.Assign("i", i.Num(10))), i.IfResult("id", i.stack(0))), env) == env + ("id" -> i.stack(0,   1)))
    env = i.eval_stmt(i.If(i.False, List(i.Skip), List(i.Assign("i", i.Num(1))), i.IfResult("id", i.stack(0))), env)
    assert(env("id").asInstanceOf[i.RVar] == i.stack(0, 0))
    assert(env("i").asInstanceOf[i.RVar].top == 1)
    val w_id = i.whileID()
    assert(i.eval_stmt(i.While(i.False, List(i.Skip), i.Counter(w_id)), env) == env + (w_id -> i.stack(0,  0)))
    env = i.eval_stmt(i.While(i.Bop(">", i.Var("i"), i.Num(0)), List(i.Assign("i", i.Num(0))), i.Counter(i.whileID())), env)
    assert(env("i").asInstanceOf[i.RVar].top == 0)
    env = Map() // clear environment
    env = i.eval_stmt(i.ArrayWithSize("arr",i.Num(10)), env)
    assert(env("arr").asInstanceOf[i.RArray].length == 10)
    assert(env("arr").asInstanceOf[i.RArray](0).top == 0)
    assert(env("arr").asInstanceOf[i.RArray](9).top == 0)
    assertThrows[java.lang.ArrayIndexOutOfBoundsException](env("arr").asInstanceOf[i.RArray](-1).top == 0)
    assertThrows[java.lang.ArrayIndexOutOfBoundsException](env("arr").asInstanceOf[i.RArray](10).top == 0)
    env = i.eval(i.parse("arr := [1,2,3,4,5,6,7,8,9,10]"), env)
    env = i.eval(i.parse("arr[0] := 2"), env)
    env = i.eval(i.parse("arr[1] := 3"), env)
    val stack_arr = env("arr").asInstanceOf[i.RArray]
    assert(stack_arr(0).length == 3)
    assert(stack_arr(0) == i.stack(0, 1, 2))
    assert(stack_arr(1).length == 3)
    assert(stack_arr(1) == i.stack(0, 2, 3))
    assert(stack_arr(2).length == 2)
    assert(stack_arr(9).length == 2)
  }


  test("Test revEval_stmt of AssignArr") {
    val ast = i.parse("arr := [1,2,3,4,5,6,7,8,9,10];arr := [0,0,0,0,0,0,0,0,0,0];arr := [1,2,3,4,5,6,7,8,9,10]")
    env = i.eval(ast, env)
    val stack_arr = env("arr").asInstanceOf[i.RArray]
    for (i <- stack_arr.indices) {
      assert(stack_arr(i).length == 4)
    }
    env = i.revEval(i.revAST(ast), env)
    val rev_stack_arr = env("arr").asInstanceOf[i.RArray]
    for (i <- rev_stack_arr.indices) {
      assert(rev_stack_arr(i).length == 1)
      assert(rev_stack_arr(i).top == 0)
    }
  }


  test("Test revEval_stmt of ArrWithSize") {
    val ast = i.parse("arr := |10|;arr := [1,2,3,4,5,6,7,8,9,10];arr := [0,0,0,0,0,0,0,0,0,0];arr := [1,2,3,4,5,6,7,8,9,10]")
    env = i.eval(ast, env)
    val stack_arr = env("arr").asInstanceOf[i.RArray]
    for (i <- stack_arr.indices) {
      assert(stack_arr(i).length == 4)
      assert(stack_arr(i).top == i+1)
    }
    env = i.revEval(i.revAST(ast), env)
    val rev_stack_arr = env("arr").asInstanceOf[i.RArray]
    for (i <- rev_stack_arr.indices) {
      assert(rev_stack_arr(i).length == 1)
      assert(rev_stack_arr(i).top == 0)
    }
  }


  test("Test revEval_stmt of UpdateArrIndex") {
    val ast = i.parse("arr := |10|;arr := [1,2,3,4,5,6,7,8,9,10];arr[0]:= 11")
    env = i.eval(ast, env)
    val stack_arr = env("arr").asInstanceOf[i.RArray]
    assert(stack_arr(0).length == 3)
    assert(stack_arr(0).top == 11)
    for (i <- 1 until stack_arr.length) {
      assert(stack_arr(i).length == 2)
      assert(stack_arr(i).top == i + 1)
    }
    env = i.revEval(i.revAST(ast), env)
    val rev_stack_arr = env("arr").asInstanceOf[i.RArray]

    for (i <- rev_stack_arr.indices) {
      assert(rev_stack_arr(i).length == 1)
      assert(rev_stack_arr(i).top == 0)
    }
  }

  test("Test revEval_stmt of If-then-else when if true") {
    val ast = i.parse(fixtures.if_true_prog)
    env = i.eval(ast, env)
    assert(env("x").asInstanceOf[i.RVar].length == 3)
    assert(env("x").asInstanceOf[i.RVar].top == 9)
    assert(env("y").asInstanceOf[i.RVar].length == 2)
    assert(env("y").asInstanceOf[i.RVar].top == 100)
    assert(env("_if1").asInstanceOf[i.RVar].length == 2)
    assert(env("_if1").asInstanceOf[i.RVar] == i.stack(0, 1))

    env = i.revEval(i.revAST(ast), env)
    assert(env("x").asInstanceOf[i.RVar].length == 1)
    assert(env("x").asInstanceOf[i.RVar].top == 0)
    assert(env("y").asInstanceOf[i.RVar].length == 1)
    assert(env("y").asInstanceOf[i.RVar].top == 0)
    assert(env("_if1").asInstanceOf[i.RVar].length == 1)
    assert(env("_if1").asInstanceOf[i.RVar] == i.stack(0))
  }


  test("Test revEval_stmt of If-then-else when if false") {
    val ast = i.parse(fixtures.if_false_prog)
    env = i.eval(ast, env)
    assert(env("x").asInstanceOf[i.RVar].length == 2)
    assert(env("x").asInstanceOf[i.RVar].top == 10)
    assert(env("y").asInstanceOf[i.RVar].length == 3)
    assert(env("y").asInstanceOf[i.RVar].top == 99)
    assert(env("_if1").asInstanceOf[i.RVar].length == 2)
    assert(env("_if1").asInstanceOf[i.RVar] == i.stack(0, 0))

    env = i.revEval(i.revAST(ast), env)
    assert(env("x").asInstanceOf[i.RVar].length == 1)
    assert(env("x").asInstanceOf[i.RVar].top == 0)
    assert(env("y").asInstanceOf[i.RVar].length == 1)
    assert(env("y").asInstanceOf[i.RVar].top == 0)
    assert(env("_if1").asInstanceOf[i.RVar].length == 1)
    assert(env("_if1").asInstanceOf[i.RVar] == i.stack(0))
  }


  test("Test evaluation of blocks") {
    assert(i.eval_bl(List(), env) == env)
    env = i.eval_bl(List(i.Assign("i10", i.Num(10)), i.Assign("i11", i.Num(11))), env)
    assert(env("i10").asInstanceOf[i.RVar] == i.stack(0, 10))
    assert(env("i10").asInstanceOf[i.RVar].top == 10)
    assert(env("i11").asInstanceOf[i.RVar] == i.stack(0, 11))
    assert(env("i11").asInstanceOf[i.RVar].top == 11)
  }

  test("Test eval function") {
    assert(i.eval(List()) == env)
    env = i.eval(List(i.Assign("i10", i.Num(10)), i.Assign("i11", i.Num(11))))
    assert(env("i10").asInstanceOf[i.RVar] == i.stack(0, 10))
    assert(env("i10").asInstanceOf[i.RVar].top == 10)
    assert(env("i11").asInstanceOf[i.RVar] == i.stack(0, 11))
    assert(env("i11").asInstanceOf[i.RVar].top == 11)
  }




  test("Test creating empty array with size should have a correct size") {
    env = i.eval(i.parse("arr := |10|"))
    assert(env("arr").asInstanceOf[i.RArray].length == 10)
  }

  test("Assigning array indexes should work correctly") {
    env = i.eval(i.parse(fixtures.arrProg1))
    assert(env("i").asInstanceOf[i.RVar].top == 10)
    assert(env("ii").asInstanceOf[i.RVar].top == 10)
    assert(env("x").asInstanceOf[i.RVar].top == 9)
    val arr = env("arr").asInstanceOf[i.RArray]
    for(i <- 0  until 10){
      assert(arr(i).top == i)
    }
  }

  test("Test creating an array with values") {
    env = i.eval(i.parse("arr := [1, 2, 3]"))
    assert(env("arr").asInstanceOf[i.RArray].length == 3)
    val arr = env("arr").asInstanceOf[i.RArray]
    assert(arr(0).top == 1)
    assert(arr(0)== i.stack(0, 1))
    assert(arr(1).top == 2)
    assert(arr(1)== i.stack(0, 2))
    assert(arr(2).top == 3)
    assert(arr(2)== i.stack(0, 3))
  }

  test("Test getting value from array indexes") {
    env = i.eval(i.parse("arr := [1, 2, 3]; i0 := arr[0]; i1 := arr[1]; i2 := arr[2]"))
    assert(env("arr").asInstanceOf[i.RArray].length == 3)
    assert(env("i0").asInstanceOf[i.RVar].top == 1)
    assert(env("i1").asInstanceOf[i.RVar].top  == 2)
    assert(env("i2").asInstanceOf[i.RVar].top  == 3)
    assertThrows[java.lang.ArrayIndexOutOfBoundsException](i.eval(i.parse("i3 := arr[3]"), env))
    assertThrows[java.lang.ArrayIndexOutOfBoundsException](i.eval(i.parse("i := arr[-1]"), env))
  }


  test("Test stack_tops function") {
    val ast = i.parse(fixtures.EX1)
    env = i.eval(ast)
    env = i.eval(i.parse("arr := [1,2,3]"), env)
    assert(i.stack_tops(env) == s"Map(fact -> 6, n -> 0, _k1 -> 3, arr -> Array[1, 2, 3])")
  }


  test("Test revEval function") {
    val ast = i.parse(fixtures.EX1)
    env = i.eval(ast)
    env = i.revEval(i.revAST(ast), env)
    assert(i.stack_tops(env) == s"Map(fact -> 0, n -> 0, _k1 -> 0)")
    val fact_stack = env("fact").asInstanceOf[i.RVar]
    assert(fact_stack.length == 1)
    val n_stack = env("n").asInstanceOf[i.RVar]
    assert(n_stack.length == 1)
  }

  test("Test forward evaluation of pre-defined program 1") {
    val ast = i.parse(fixtures.EX1)
    val env = i.eval(ast)
    assert(env("fact").asInstanceOf[i.RVar] == i.stack(0, 1, 3, 6, 6))
    assert(env("n").asInstanceOf[i.RVar] == i.stack(0, 3, 2, 1, 0))
    assert(env("_k1").asInstanceOf[i.RVar] == i.stack(0, 3))
  }



  test("Test forward evaluation of pre-defined program 2") {
    val ast = i.parse(fixtures.EX2)
    val env = i.eval(ast)
    assert(env("a").asInstanceOf[i.RVar] == i.stack(0, 49, 21, 14, 7))
    assert(env("b").asInstanceOf[i.RVar] == i.stack(0, 28, 7))
    assert(env("_if1").asInstanceOf[i.RVar] == i.stack(0, 1, 0, 1, 1))
    assert(env("_k1").asInstanceOf[i.RVar] == i.stack(0, 4))
  }

  test("Test forward evaluation of pre-defined program 3") {
    val ast = i.parse(fixtures.EX3)
    val env = i.eval(ast)
    assert(env("x").asInstanceOf[i.RVar] == i.stack(0, 12,6,3,10,5,16,8,4,2,1))
    assert(env("r").asInstanceOf[i.RVar] == i.stack(0, 12, 10, 8, 6, 4, 2, 0, 6, 4, 2, 0, 3, 1, 10, 8, 6, 4, 2, 0, 5, 3, 1, 16, 14, 12, 10, 8, 6, 4, 2, 0, 8, 6, 4, 2, 0, 4, 2, 0, 2, 0))
    assert(env("_if1").asInstanceOf[i.RVar] == i.stack(0,  1, 1, 0, 1, 0, 1, 1, 1, 1))
    assert(env("_k1").asInstanceOf[i.RVar] == i.stack(0,  9))
    assert(env("_k2").asInstanceOf[i.RVar] == i.stack(0,  6, 3, 1, 5, 2, 8, 4, 2, 1))

  }

  test("Test forward evaluation of pre-defined program 4") {
    val ast = i.parse(fixtures.EX4)
    val env = i.eval(ast)
    assert(env("x").asInstanceOf[i.RVar] == i.stack(0, 13))
    assert(env("r").asInstanceOf[i.RVar] == i.stack(0, 13, 11, 9, 7, 5, 3, 1, 13, 10, 7, 4, 1, 13, 9, 5, 1, 13, 8, 3, 13, 7, 1))
    assert(env("isprime").asInstanceOf[i.RVar] == i.stack(0, 1))
    assert(env("limit").asInstanceOf[i.RVar] == i.stack(0, 7))
    assert(env("factor").asInstanceOf[i.RVar] == i.stack(0, 2, 3, 4, 5, 6, 7))
    assert(env("_k1").asInstanceOf[i.RVar] == i.stack(0, 5))
    assert(env("_k2").asInstanceOf[i.RVar] == i.stack(0, 6, 4, 3, 2, 2))
    assert(env("_if1").asInstanceOf[i.RVar] == i.stack(0, 0,0,0,0,0))
  }

  test("Test forward evaluation of pre-defined program 5") {
    val ast = i.parse(fixtures.EX5)
    val env = i.eval(ast)
    assert(env("a").asInstanceOf[i.RVar] == i.stack(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89))
    assert(env("b").asInstanceOf[i.RVar] == i.stack(0, 1,2,3,5,8,13,21,34,55,89,144))
    assert(env("i").asInstanceOf[i.RVar] == i.stack(0, 0,1,2,3,4,5,6,7,8,9,10))
    assert(env("tmp").asInstanceOf[i.RVar] == i.stack(0, 1,1,2,3,5,8,13,21,34,55))
    assert(env("_k1").asInstanceOf[i.RVar] == i.stack(0, 10))
  }

  test("Test forward evaluation of pre-defined reverse_arr_prog") {
    val ast = i.parse(fixtures.reverse_arr_prog)
    val env = i.eval(ast)
    val arr = env("arr").asInstanceOf[i.RArray]
    assert(arr(0) == i.stack(0, 1, 5))
    assert(arr(1) == i.stack(0, 2, 4))
    assert(arr(2) == i.stack(0,    3))
    assert(arr(3) == i.stack(0, 4, 2))
    assert(arr(4) == i.stack(0, 5, 1))
    assert(env("_k1").asInstanceOf[i.RVar] == i.stack(0, 2))
  }

  test("Test backward evaluation of pre-defined programs") {
    for(p <- fixtures.allExamples){
      // Forward evaluation
      val ast = i.parse(p)
      val env = i.eval(ast)

      // Backward evaluation
      val rev_ast = i.revAST(ast)
      val rev_env = i.revEval(rev_ast, env)
      rev_env.values.foreach(s => assert(stackOnlyHasZero(s)))
    }
  }


  def stackOnlyHasZero(stack: Any): Boolean = stack match {
    case s: i.RVar => s.length == 1 && s.top == 0
    case s: i.RArray => s.forall(i => i.length == 1 && i.top == 0)
    case _ => throw new RuntimeException("stackOnlyHasZero function can only check RVar and RArray types")
  }
  
}
