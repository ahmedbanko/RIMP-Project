package rimp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class InterpreterTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach {

  val interp = new Interpreter()
  type Env = Map[String, Any]
  var env: Env = Map()

  override protected def afterEach(): Unit = {
    super.afterEach()
    env = Map() // clear environment
  }

  var counter = -1
  def counterID(): String = {
    s"while_${counter + 1}_k"
  }



  val prog = "arr := [1+1, 2, 3]; i1before := arr[1]; arr[1] := 10; i1after := arr[1]"
  val exampleProg1 =
    """fact := 1;
    n := 3;
    while (!n > 0) do {
        fact := !n * !fact;
        n := !n - 1}
        """

  val if_true_prog =
    """
      x := 10;
      y := 100;
      if (!x > 0) then {
          x := !x - 1
      }else{
          y := !y - 1
      }
      """

  val if_false_prog =
    """
      x := 10;
      y := 100;
      if (!x > 100) then {
          x := !x - 1
      }else{
          y := !y - 1
      }
      """
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
    assert(interp.eval_stmt(interp.While(interp.False, List(interp.Skip), interp.Counter(counterID(), 0)), env) == env)
    env = interp.eval_stmt(interp.While(interp.Bop(">", interp.Var("i"), interp.Num(0)), List(interp.Assign("i", interp.Num(0))), interp.Counter(counterID(), 0)), env)
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
    val ast = interp.parse(if_true_prog)
    val my_if = ast.filter(x => x.isInstanceOf[interp.If]).head
    val my_if_id = my_if.asInstanceOf[interp.If].boolStack.id
    val my_if_int = my_if_id.split("_")(1)
    env = interp.eval(ast, env)
    assert(env("x").asInstanceOf[interp.RVar].length == 3)
    assert(env("x").asInstanceOf[interp.RVar].top == 9)
    assert(env("y").asInstanceOf[interp.RVar].length == 2)
    assert(env("y").asInstanceOf[interp.RVar].top == 100)
    assert(env(s"if_${my_if_int}_result").asInstanceOf[interp.RVar].length == 2)
    assert(env(s"if_${my_if_int}_result").asInstanceOf[interp.RVar] == interp.stack(1))

    env = interp.revEval(interp.revAST(ast), env)
    assert(env("x").asInstanceOf[interp.RVar].length == 1)
    assert(env("x").asInstanceOf[interp.RVar].top == 0)
    assert(env("y").asInstanceOf[interp.RVar].length == 1)
    assert(env("y").asInstanceOf[interp.RVar].top == 0)
    assert(env(s"if_${my_if_int}_result").asInstanceOf[interp.RVar].length == 1)
    assert(env(s"if_${my_if_int}_result").asInstanceOf[interp.RVar] == interp.stack())
  }


  test("Test revEval_stmt of If-then-else when if false") {
    val ast = interp.parse(if_false_prog)
    val my_if = ast.filter(x => x.isInstanceOf[interp.If]).head
    val my_if_id = my_if.asInstanceOf[interp.If].boolStack.id
    val my_if_int = my_if_id.split("_")(1)
    env = interp.eval(ast, env)
    assert(env("x").asInstanceOf[interp.RVar].length == 2)
    assert(env("x").asInstanceOf[interp.RVar].top == 10)
    assert(env("y").asInstanceOf[interp.RVar].length == 3)
    assert(env("y").asInstanceOf[interp.RVar].top == 99)
    assert(env(s"if_${my_if_int}_result").asInstanceOf[interp.RVar].length == 2)
    assert(env(s"if_${my_if_int}_result").asInstanceOf[interp.RVar] == interp.stack(0))

    env = interp.revEval(interp.revAST(ast), env)
    assert(env("x").asInstanceOf[interp.RVar].length == 1)
    assert(env("x").asInstanceOf[interp.RVar].top == 0)
    assert(env("y").asInstanceOf[interp.RVar].length == 1)
    assert(env("y").asInstanceOf[interp.RVar].top == 0)
    assert(env(s"if_${my_if_int}_result").asInstanceOf[interp.RVar].length == 1)
    assert(env(s"if_${my_if_int}_result").asInstanceOf[interp.RVar] == interp.stack())
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


  val arrProg =
    """arr := |10|;
       i := 0;
       while (!i < 10) do {
        arr[!i] := !i;
        i := !i + 1
       };
       ii := 0;
       while (!ii < 10) do {
          x := arr[!ii];
            ii := !ii + 1
           }"""

  test("Test creating empty array with size should have a correct size") {
    env = interp.eval(interp.parse("arr := |10|"))
    assert(env("arr").asInstanceOf[interp.RArray].length == 10)
  }

  test("Assigning array indexes should work correctly") {
    env = interp.eval(interp.parse(arrProg))
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
    val ast = interp.parse(exampleProg1)
    val my_while = ast.filter(x => x.isInstanceOf[interp.While]).head
    val my_while_id = my_while.asInstanceOf[interp.While].counter.id
    val my_while_int = my_while_id.split("_")(1)
    env = interp.eval(ast)
    env = interp.eval(interp.parse("arr := [1,2,3]"), env)

    assert(interp.stack_tops(env) == s"Map(fact -> 6, n -> 0, W_${my_while_int}_k -> 3, arr -> Array[1, 2, 3])")
  }


  test("Test revEval function") {
    val ast = interp.parse(exampleProg1)
    val my_while = ast.filter(x => x.isInstanceOf[interp.While]).head
    val my_while_id = my_while.asInstanceOf[interp.While].counter.id
    val my_while_int = my_while_id.split("_")(1)
    env = interp.eval(ast)
    env = interp.revEval(interp.revAST(ast), env)
    assert(interp.stack_tops(env) == s"Map(fact -> 0, n -> 0, W_${my_while_int}_k -> 0)")
    val fact_stack = env("fact").asInstanceOf[interp.RVar]
    assert(fact_stack.length == 1)
    val n_stack = env("n").asInstanceOf[interp.RVar]
    assert(n_stack.length == 1)
  }
  
}
