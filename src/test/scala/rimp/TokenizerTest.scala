package rimp

import org.scalatest.funsuite.AnyFunSuite

//
class TokenizerTest extends AnyFunSuite {
  val tk: Tokenizer = new Tokenizer
  val prog = "arr := [1, 2, 3]; i1before := arr[1]; arr[1] := 10; i1after := arr[1]"
  val exampleProg1 =
    """fact := 1;
    n := 3;
    while (!n > 0) do {
        fact := !n * !fact;
        n := !n - 1}
        """

  val exampleProg2 =
    """a := 49;
                b := 28;
                while ~(!a = !b) do
                if !a > !b
                then a := !a - !b
                else b := !b - !a""".stripMargin

  val exampleProg3 =
    """x := 12;
    while !x > 1 do {
        r := !x;
        (while !r > 1 do
            r := !r - 2);
        if !r = 0
        then x := !x / 2
        else x := 3 * !x + 1}""".stripMargin


  val exampleProg4 =
    """x := 13;
    factor := 2;
    isprime := 1;
    limit := !x / 2 + 1;
    while !factor < !limit do {
        r := !x;
        (while !r > !factor - 1 do
            r := !r - !factor);
        (if !r = 0 then isprime := 0 else skip);
        factor := !factor + 1}""".stripMargin

  val exampleProg5 =
    """n := 10;
    a := 1;
    b := 1;
    i := 0;
    while !i < !n do {
        tmp := !a;
        a := !b;
        b := !tmp + !a;
        i := !i + 1}""".stripMargin
  val list = List(prog, exampleProg1, exampleProg2, exampleProg3, exampleProg4, exampleProg5)

  test("Test tokenizer.tokenize should return expected value") {
    val prog_expeted = List(tk.T_ID("arr"), tk.T_OP(":="),
      tk.T_LSQRB, tk.T_NUM(1), tk.T_COMMA, tk.T_NUM(2),
      tk.T_COMMA, tk.T_NUM(3), tk.T_RSQRB, tk.T_SEMI,
      tk.T_ID("i1before"), tk.T_OP(":="), tk.T_ID("arr"),
      tk.T_LSQRB, tk.T_NUM(1), tk.T_RSQRB, tk.T_SEMI,
      tk.T_ID("arr"), tk.T_LSQRB, tk.T_NUM(1), tk.T_RSQRB,
      tk.T_OP(":="), tk.T_NUM(10), tk.T_SEMI, tk.T_ID("i1after"),
      tk.T_OP(":="), tk.T_ID("arr"), tk.T_LSQRB, tk.T_NUM(1), tk.T_RSQRB)

    val exampleProg1Expected = List(tk.T_ID("fact"), tk.T_OP(":="), tk.T_NUM(1),
      tk.T_SEMI, tk.T_ID("n"), tk.T_OP(":="), tk.T_NUM(3), tk.T_SEMI, tk.T_KWD("while"),
      tk.T_LBRACK, tk.T_OP("!"), tk.T_ID("n"),tk.T_OP(">"),tk.T_NUM(0), tk.T_RBRACK,
      tk.T_KWD("do"), tk.T_LPAREN, tk.T_ID("fact"), tk.T_OP(":="), tk.T_OP("!"),
      tk.T_ID("n"), tk.T_OP("*"), tk.T_OP("!"), tk.T_ID("fact"),tk.T_SEMI, tk.T_ID("n"),
      tk.T_OP(":="), tk.T_OP("!"),tk.T_ID("n"), tk.T_OP("-"),tk.T_NUM(1), tk.T_RPAREN)
    assert(tk.tokenize(prog) == prog_expeted)
    assert(tk.tokenize(exampleProg1) == exampleProg1Expected)
    assert(tk.tokenize(prog).nonEmpty)
    assert(tk.tokenize(exampleProg1).nonEmpty)
  }

//
////  class TokenizerPropertyTest extends AnyPropSpec {
////    val tk: Tokenizer = new Tokenizer
////    val prog = "arr := [1, 2, 3]; i1before := arr[1]; arr[1] := 10; i1after := arr[1]"
////    val exampleProg1 =
////      """fact := 1;
////    n := 3;
////    while (!n > 0) do {
////        fact := !n * !fact;
////        n := !n - 1}
////        """
////
////    val exampleProg2 =
////      """a := 49;
////                b := 28;
////                while ~(!a = !b) do
////                if !a > !b
////                then a := !a - !b
////                else b := !b - !a""".stripMargin
////
////    val exampleProg3 =
////      """x := 12;
////    while !x > 1 do {
////        r := !x;
////        (while !r > 1 do
////            r := !r - 2);
////        if !r = 0
////        then x := !x / 2
////        else x := 3 * !x + 1}""".stripMargin
////
////
////    val exampleProg4 =
////      """x := 13;
////    factor := 2;
////    isprime := 1;
////    limit := !x / 2 + 1;
////    while !factor < !limit do {
////        r := !x;
////        (while !r > !factor - 1 do
////            r := !r - !factor);
////        (if !r = 0 then isprime := 0 else skip);
////        factor := !factor + 1}""".stripMargin
////
////    val exampleProg5 =
////      """n := 10;
////    a := 1;
////    b := 1;
////    i := 0;
////    while !i < !n do {
////        tmp := !a;
////        a := !b;
////        b := !tmp + !a;
////        i := !i + 1}""".stripMargin
////    val list = List(prog, exampleProg1, exampleProg2, exampleProg3, exampleProg4, exampleProg5)
////
////    //  TODO:  not really working
////    property("Tokenizer should not return whitespaces") {
////      list.forall(p => tk.tokenize(p).size == -1)
////
////    }
////
////    property("Token Classification") {
////      forAll { code: String =>
////        tk.tokenize(code).contains("heeee")
////      }
////    }
////  }

//  import org.scalacheck.Prop.forAll
//  import org.scalacheck.Properties
//
//  class ExampleSpecification extends Properties("Example") {
//
//    // Property to test
//    property("reversing a list twice should give the original list") = forAll { l: List[Int] =>
//      l.reverse == l
//    }
//  }


}