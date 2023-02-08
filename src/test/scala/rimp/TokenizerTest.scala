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

  test("Test partial function") {
    assert(tk.token.apply("semicolon", ";") == tk.T_SEMI)
    assert(tk.token.apply("comma", ",") == tk.T_COMMA)
    assert(tk.token.apply("parenthesis", "{") == tk.T_LPAREN)
    assert(tk.token.apply("parenthesis", "}") == tk.T_RPAREN)
    assert(tk.token.apply("brackets", "(") == tk.T_LBRACK)
    assert(tk.token.apply("brackets", ")") == tk.T_RBRACK)
    assert(tk.token.apply("sqr_brackets", "[") == tk.T_LSQRB)
    assert(tk.token.apply("sqr_brackets", "]") == tk.T_RSQRB)
    assert(tk.token.apply("bar", "|") == tk.T_BAR)
    assert(tk.token.apply("id", "someID") == tk.T_ID("someID"))
    assert(tk.token.apply("operation", "someOP") == tk.T_OP("someOP"))
    assert(tk.token.apply("number", "1") == tk.T_NUM(1))
    assert(tk.token.apply("keyword", "if") == tk.T_KWD("if"))
  }

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
}