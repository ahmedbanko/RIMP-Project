package rimp

import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.funsuite.AnyFunSuite

class InterpreterTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach{

  val interp = new Interpreter()
  type Env = Map[String, Any]
  var env: Env = Map()

  override protected def afterEach(): Unit = {
    super.afterEach()
    env = Map() // clear environment
  }



  val prog = "arr := [1, 2, 3]; i1before := arr[1]; arr[1] := 10; i1after := arr[1]"
  val exampleProg1 =
    """fact := 1;
    n := 3;
    while (!n > 0) do {
        fact := !n * !fact;
        n := !n - 1}
        """

  test("Test strList2IntList function") {
    val listIn = "List(1, 2, 3)"
    val listOut = List(1, 2, 3)
    assert(interp.strList2IntList(listIn) == listOut)
  }

  test("Test evaluation of arithmatic expressions") {
    assert(interp.eval_aexp(interp.Num(1), env) == 1)
    assertThrows[java.util.NoSuchElementException](interp.eval_aexp(interp.Var("x"), env))
    env = env + ("x" -> 10)
    assert(interp.eval_aexp(interp.Var("x"), env) == 10)
    assertThrows[java.util.NoSuchElementException](interp.eval_aexp(interp.ArrayVar("arr", interp.Num(0)), env) == 1)
    env = env + ("arr" -> List(1,2,3))
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

  test("Test evaluation of array values") {
    val aExpIn = List(interp.Aop("-", interp.Num(10), interp.Num(1)), interp.Num(8), interp.Num(7))
    assert(interp.eval_arrVals(aExpIn, env) == List(9, 8, 7))
  }

  test("Test evaluation of statements") {
    assert(interp.eval_stmt(interp.Skip, env) == env)
    assertThrows[java.util.NoSuchElementException](env("x") == 10)
    env = interp.eval_stmt(interp.Assign("x", interp.Num(10)), env)
    assert(env("x") == 10)
    assertThrows[java.util.NoSuchElementException](env("arr") == List(1,2,3))
    env = interp.eval_stmt(interp.AssignArr("arr", List(interp.Num(10), interp.Num(9), interp.Num(8))), env)
    assert(env("arr") == List(10, 9, 8))
    val arr = interp.strList2IntList(env("arr").toString)
    assert(arr.head == 10)
    env = interp.eval_stmt(interp.UpdateArrIndex("arr", interp.Num(0),interp.Num(99)), env)
    val arr2 = interp.strList2IntList(env("arr").toString)
    assert(arr2.head != 10 && arr2.head == 99)
    env = Map() // clear environment
    assert(interp.eval_stmt(interp.If(interp.True, List(interp.Skip), List(interp.Assign("i", interp.Num(10)))), env) == env)
    env = interp.eval_stmt(interp.If(interp.False, List(interp.Skip), List(interp.Assign("i", interp.Num(1)))), env)
    assert(env("i").asInstanceOf[Int] == 1)
    assert(interp.eval_stmt(interp.While(interp.False, List(interp.Skip)), env) == env)
    env = interp.eval_stmt(interp.While(interp.Bop(">", interp.Var("i"), interp.Num(0)), List(interp.Assign("i", interp.Num(0)))), env)
    assert(env("i").asInstanceOf[Int] == 0)
  }

  test("Test evaluation of blocks") {
    assert(interp.eval_bl(List(), env) == env)
    env = interp.eval_bl(List(interp.Assign("i10", interp.Num(10)), interp.Assign("i11", interp.Num(11))), env)
    assert(env("i10").asInstanceOf[Int] == 10)
    assert(env("i11").asInstanceOf[Int] == 11)
  }

  test("Test eval function") {
    assert(interp.eval(List()) == env)
    env = interp.eval(List(interp.Assign("i10", interp.Num(10)), interp.Assign("i11", interp.Num(11))))
    assert(env("i10").asInstanceOf[Int] == 10)
    assert(env("i11").asInstanceOf[Int] == 11)
  }

}
