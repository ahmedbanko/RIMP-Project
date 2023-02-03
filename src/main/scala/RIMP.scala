

class RIMP {

  //  --------- Lexer -------------

  import scala.language.implicitConversions
  abstract class Rexp
  case object ZERO extends Rexp
  case object ONE extends Rexp
  case class CHAR(c: Char) extends Rexp
  case class ALT(r1: Rexp, r2: Rexp) extends Rexp
  case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
  case class STAR(r: Rexp) extends Rexp
  case class RECD(x: String, r: Rexp) extends Rexp // records for extracting strings or tokens
  case class NTIMES(r: Rexp, n: Int) extends Rexp // exactly n‐times
  case class RANGE(cs: Set[Char]) extends Rexp // a set of characters—for character ranges
  case class PLUS(r: Rexp) extends Rexp // one or more times r
  case class OPTIONAL(r: Rexp) extends Rexp
  case class CFUN(f: Char => Boolean) extends Rexp

  // values
  abstract class Val
  case object Empty extends Val
  case class Chr(c: Char) extends Val
  case class Sequ(v1: Val, v2: Val) extends Val
  case class Left(v: Val) extends Val
  case class Right(v: Val) extends Val
  case class Stars(vs: List[Val]) extends Val
  case class Rec(x: String, v: Val) extends Val
  case class Rng(vs: Set[Val]) extends Val
  case class Pls(v: List[Val]) extends Val
  case class Opt(v: Val) extends Val
  case class Nts(v: List[Val]) extends Val


  // some convenience for typing in regular expressions
  def charlist2rexp(s: List[Char]): Rexp = s match {
    case Nil => ONE
    case c :: Nil => CHAR(c)
    case c :: s => SEQ(CHAR(c), charlist2rexp(s))
  }

  implicit def string2rexp(s: String): Rexp =
    charlist2rexp(s.toList)


  implicit def RexpOps(r: Rexp) = new {
    def |(s: Rexp) = ALT(r, s)
    def % = STAR(r)
    def ~(s: Rexp) = SEQ(r, s)
  }

  implicit def stringOps(s: String) = new {
    def |(r: Rexp) = ALT(s, r)
    def |(r: String) = ALT(s, r)
    def % = STAR(s)
    def ~(r: Rexp) = SEQ(s, r)
    def $(r: Rexp) = RECD(s, r)
  }

  def nullable(r: Rexp): Boolean = r match {
    case ZERO => false
    case ONE => true
    case CHAR(_) => false
    case ALT(r1, r2) => nullable(r1) || nullable(r2)
    case SEQ(r1, r2) => nullable(r1) && nullable(r2)
    case STAR(_) => true
    case RECD(_, r1) => nullable(r1)

    case NTIMES(r, n) => if (n == 0) true else nullable(r)
    case RANGE(cs) => false
    case PLUS(r) => nullable(r)
    case OPTIONAL(r) => true
    case CFUN(f) => false
  }

  def der(c: Char, r: Rexp): Rexp = r match {
    case ZERO => ZERO
    case ONE => ZERO
    case CHAR(d) => if (c == d) ONE else ZERO
    case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
    case SEQ(r1, r2) =>
      if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
      else SEQ(der(c, r1), r2)
    case STAR(r) => SEQ(der(c, r), STAR(r))
    case RECD(_, r1) => der(c, r1)

    case NTIMES(r, n) => if (n == 0) ZERO else SEQ(der(c, r), NTIMES(r, n - 1))
    case RANGE(cs) => if (cs.contains(c)) ONE else ZERO
    case PLUS(r) => SEQ(der(c, r), STAR(r))
    case OPTIONAL(r) => der(c, r)
    case CFUN(f) => if (f(c)) ONE else ZERO
  }


  // extracts a string from a value
  def flatten(v: Val): String = v match {
    case Empty => ""
    case Chr(c) => c.toString
    case Left(v) => flatten(v)
    case Right(v) => flatten(v)
    case Sequ(v1, v2) => flatten(v1) ++ flatten(v2)
    case Stars(vs) => vs.map(flatten).mkString
    case Rec(_, v) => flatten(v)

    case Rng(vs) => vs.map(flatten).mkString
    case Pls(vs) => vs.map(flatten).mkString
    case Opt(v) => flatten(v)
    case Nts(vs) => vs.map(flatten).mkString
  }


  // extracts an environment from a value;
  // used for tokenising a string
  def env(v: Val): List[(String, String)] = v match {
    case Empty => Nil
    case Chr(c) => Nil
    case Left(v) => env(v)
    case Right(v) => env(v)
    case Sequ(v1, v2) => env(v1) ::: env(v2)
    case Stars(vs) => vs.flatMap(env)
    case Rec(x, v) => (x, flatten(v)) :: env(v)

    case Rng(vs) => vs.flatMap(env).toList
    case Pls(vs) => vs.flatMap(env)
    case Opt(v) => env(v)
    case Nts(vs) => vs.flatMap(env)
  }


  // The injection and mkeps part of the lexer
  //===========================================

  def mkeps(r: Rexp): Val = r match {
    case ONE => Empty
    case ALT(r1, r2) =>
      if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
    case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
    case STAR(r) => Stars(Nil)
    case RECD(x, r) => Rec(x, mkeps(r))
    case PLUS(r) => Stars(List(mkeps(r)))
    case OPTIONAL(r) => Stars(Nil)
    case NTIMES(r, n) => Stars(List.fill(n)(mkeps(r)))
  }

  def inj(r: Rexp, c: Char, v: Val): Val = (r, v) match {
    case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1) :: vs)
    case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
    case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
    case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
    case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
    case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
    case (CHAR(d), Empty) => Chr(c)
    case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))

    case (RANGE(_), Empty) => Chr(c)
    case (PLUS(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1) :: vs)
    case (OPTIONAL(r), v) => Stars(inj(r, c, v) :: Nil)
    case (NTIMES(r, n), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1) :: vs)
  }

  // lexing functions without simplification
  def lex(r: Rexp, s: List[Char]): Val = s match {
    case Nil => if (nullable(r)) mkeps(r) else {
      throw new Exception("lexing error")
    }
    case c :: cs => inj(r, c, lex(der(c, r), cs))
  }

  def lexing(r: Rexp, s: String) =
    env(lex(r, s.toList))


  // some "rectification" functions for simplification
  def F_ID(v: Val): Val = v

  def F_RIGHT(f: Val => Val) = (v: Val) => Right(f(v))

  def F_LEFT(f: Val => Val) = (v: Val) => Left(f(v))

  def F_ALT(f1: Val => Val, f2: Val => Val) = (v: Val) => v match {
    case Right(v) => Right(f2(v))
    case Left(v) => Left(f1(v))
  }

  def F_SEQ(f1: Val => Val, f2: Val => Val) = (v: Val) => v match {
    case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
  }

  def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) =
    (v: Val) => Sequ(f1(Empty), f2(v))

  def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) =
    (v: Val) => Sequ(f1(v), f2(Empty))

  def F_ERROR(v: Val): Val = throw new Exception("error")

  // simplification
  def simp(r: Rexp): (Rexp, Val => Val) = r match {
    case ALT(r1, r2) => {
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (ZERO, _) => (r2s, F_RIGHT(f2s))
        case (_, ZERO) => (r1s, F_LEFT(f1s))
        case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
        else (ALT(r1s, r2s), F_ALT(f1s, f2s))
      }
    }
    case SEQ(r1, r2) => {
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (ZERO, _) => (ZERO, F_ERROR)
        case (_, ZERO) => (ZERO, F_ERROR)
        case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
        case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
        case _ => (SEQ(r1s, r2s), F_SEQ(f1s, f2s))
      }
    }
    case r => (r, F_ID)
  }

  // lexing functions including simplification
  def lex_simp(r: Rexp, s: List[Char]): Val = s match {
    case Nil => if (nullable(r)) mkeps(r) else {
      throw new Exception("lexing error")
    }
    case c :: cs => {
      val (r_simp, f_simp) = simp(der(c, r))
      inj(r, c, f_simp(lex_simp(r_simp, cs)))
    }
  }

  def lexing_simp(r: Rexp, s: String) =
    env(lex_simp(r, s.toList))


  def Range(s: List[Char]): Rexp = s match {
    case Nil => ZERO
    case c :: Nil => CHAR(c)
    case c :: s => ALT(CHAR(c), Range(s))
  }


  val KEYWORD: Rexp = "skip" | "while" | "do" | "if" | "then" | "else" | "true" | "false"
  val OP: Rexp = "+" | "-" | "*" | "%" | "/" | "=" | "!=" | ">" | "<" | "<=" | ">=" | ":=" | "!" | "~"
  val alphabet: String = ('A' to 'Z').toList.mkString ++ ('a' to 'z').toList.mkString
  val LETTER: Rexp = RANGE(alphabet.toSet)
  val SYM: Rexp = RANGE((alphabet ++ """._><=;,\:""").toSet)
  val WHITESPACE: Rexp = PLUS(" " | "\n" | "\t" | "\r")
  val DIGIT: Rexp = RANGE("0123456789".toSet)
//  val STRING: Rexp = "\"" ~ (SYM | WHITESPACE | DIGIT).% ~ "\""
  val RSQRB: Rexp = "]"
  val LSQRB: Rexp = "["
  val RPAREN: Rexp = "}"
  val LPAREN: Rexp = "{"
  val RBRACK: Rexp = ")"
  val LBRACK: Rexp = "("
  val SEMI: Rexp = ";"
  val COMMA: Rexp = ","
  val ID: Rexp = LETTER ~ ("_" | LETTER | DIGIT).%
  val NUM: Rexp = "0" | (RANGE("123456789".toSet) ~ STAR(DIGIT))

  val RIMP_REGS = (("keyword" $ KEYWORD) |
    ("id" $ ID) |
    ("operation" $ OP) |
    ("number" $ NUM) |
    ("semicolon" $ SEMI) |
    ("comma" $ COMMA) |
    ("parenthesis" $ (LPAREN | RPAREN)) |
    ("brackets" $ (RBRACK | LBRACK)) |
    ("sqr_brackets" $ (RSQRB | LSQRB)) |
    ("whitespace" $ WHITESPACE) |
    ("symbol" $ SYM) |
    ("digit" $ DIGIT)).%





  //  --------- Parser -------------


  //  import $file.token
  //  import token._

  case class ~[+A, +B](x: A, y: B)

  // constraint for the input
  type IsSeq[A] = A => Seq[_]

  type Tokens = Seq[Token]

  abstract class Parser[I: IsSeq, T] {
    def parse(in: I): Set[(T, I)]

    def parse_all(in: I): Set[T] =
      for ((hd, tl) <- parse(in);
           if tl.isEmpty) yield hd
  }

  // parser combinators

  // sequence parser
  class SeqParser[I: IsSeq, T, S](p: => Parser[I, T],
                                  q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
    def parse(in: I) =
      for ((hd1, tl1) <- p.parse(in);
           (hd2, tl2) <- q.parse(tl1)) yield (new ~(hd1, hd2), tl2)
  }

  // alternative parser
  class AltParser[I: IsSeq, T](p: => Parser[I, T],
                               q: => Parser[I, T]) extends Parser[I, T] {
    def parse(in: I) = p.parse(in) ++ q.parse(in)
  }

  // map parser
  class MapParser[I: IsSeq, T, S](p: => Parser[I, T],
                                  f: T => S) extends Parser[I, S] {
    def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
  }

  case class TokenParser(sin: String) extends Parser[Tokens, Token] {
    def parse(in: Tokens): Set[(Token, Tokens)] = in match {
      case T_LBRACK :: tail if (sin == "(") => Set((T_LBRACK, tail))
      case T_RBRACK :: tail if (sin == ")") => Set((T_RBRACK, tail))
      case T_LSQRB :: tail if (sin == "[") => Set((T_LSQRB, tail))
      case T_RSQRB :: tail if (sin == "]") => Set((T_RSQRB, tail))
      case T_LPAREN :: tail if (sin == "{") => Set((T_LPAREN, tail))
      case T_RPAREN :: tail if (sin == "}") => Set((T_RPAREN, tail))
      case T_STR(s) :: tail if (s == sin) => Set((T_STR(s), tail))
      case T_ID(s) :: tail if (s == sin) => Set((T_ID(s), tail))
      case T_OP(s) :: tail if (s == sin) => Set((T_OP(s), tail))
      case T_NUM(n) :: tail if (n.toString == sin) => Set((T_NUM(n), tail))
      case T_KWD(s) :: tail if (s == sin) => Set((T_KWD(s), tail))
      case _ => Set()
    }
  }


  case object StrParser extends Parser[Tokens, String] {
    def parse(in: Tokens) = in match {
      case T_STR(s) :: tail => Set((s, tail))
      case _ => Set()
    }
  }


  case object CommaParser extends Parser[Tokens, String] {
    def parse(in: Tokens) = in match {
      case T_COMMA :: tail => Set((",", tail))
      case _ => Set()
    }
  }

  case object SEMIParser extends Parser[Tokens, String] {
    def parse(in: Tokens) = in match {
      case T_SEMI :: tail => Set((";", tail))
      case _ => Set()
    }
  }


  case object IdParser extends Parser[Tokens, String] {
    def parse(in: Tokens) = in match {
      case T_ID(s) :: tail => Set((s, tail))
      case _ => Set()
    }
  }


  case object NumParser extends Parser[Tokens, Int] {
    def parse(in: Tokens) = in match {
      case T_NUM(n) :: tail => Set((n, tail))
      case _ => Set()
    }
  }



  implicit def parser_interpolation(sc: StringContext) = new {
    def p(args: Any*) = TokenParser(sc.s(args: _*))
  }

  // more convenient syntax for parser combinators
  implicit def ParserOps[I: IsSeq, T](p: Parser[I, T]) = new {
    def ||(q: => Parser[I, T]) = new AltParser[I, T](p, q)
    def ~[S](q: => Parser[I, S]) = new SeqParser[I, T, S](p, q)
    def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
  }

  // the abstract syntax trees for the WHILE language
  abstract class Stmt
  abstract class AExp
  abstract class BExp
//  abstract class ArrExp


  type Block = List[Stmt]
  type ArrBlock = List[Int]

  case object Skip extends Stmt
  case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
  case class While(b: BExp, bl: Block) extends Stmt
  case class Assign(s: String, a: AExp) extends Stmt
  case class ArrVar(values: List[Int]) extends Stmt


  case class Var(s: String) extends AExp
  case class Num(i: Int) extends AExp
  case class Aop(o: String, a1: AExp, a2: AExp) extends AExp


  case object True extends BExp
  case object False extends BExp
  case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
  case class Not(b: BExp) extends BExp



  // arithmetic expressions
  lazy val AExp: Parser[Tokens, AExp] =
    (Te ~ p"+" ~ AExp).map[AExp] { case x ~ _ ~ z => Aop("+", x, z) } ||
      (Te ~ p"-" ~ AExp).map[AExp] { case x ~ _ ~ z => Aop("-", x, z) } || Te
  lazy val Te: Parser[Tokens, AExp] =
    (Fa ~ p"*" ~ Te).map[AExp] { case x ~ _ ~ z => Aop("*", x, z) } ||
      (Fa ~ p"/" ~ Te).map[AExp] { case x ~ _ ~ z => Aop("/", x, z) } ||
      (Fa ~ p"%" ~ Te).map[AExp] { case x ~ _ ~ z => Aop("%", x, z) } || Fa
  lazy val Fa: Parser[Tokens, AExp] =
    (p"(" ~ AExp ~ p")").map { case _ ~ y ~ _ => y } ||
      (p"!" ~ IdParser).map{ case _ ~ x  => Var(x)} || NumParser.map(Num)

 // TODO: fix when array is empty AND add array assignment
  lazy val ArrBlock: Parser[Tokens, ArrBlock] =
    (p"[" ~ ArrVals ~ p"]").map { case _ ~ y ~ _ => y }


  lazy val ArrVals: Parser[Tokens, ArrBlock] =
    (NumParser ~ CommaParser ~ ArrVals).map[ArrBlock] { case x ~ _ ~ z => x :: z } ||
      NumParser.map(x => List(x))


  // boolean expressions with some simple nesting
  lazy val BExp: Parser[Tokens, BExp] =
    (AExp ~ p"=" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("=", x, z) } ||
      (AExp ~ p"!=" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("!=", x, z) } ||
      (AExp ~ p"<" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("<", x, z) } ||
      (AExp ~ p">" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop(">", x, z) } ||
      (AExp ~ p">=" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop(">=", x, z) } ||
      (AExp ~ p"<=" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("<=", x, z) } ||
      (p"true").map[BExp] { _ => True } ||
      (p"false").map[BExp] { _ => False } ||
      (p"~" ~ BExp).map[BExp] {case _ ~ x => Not(x)} ||
      (p"(" ~ BExp ~ p")").map[BExp] { case _ ~ x ~ _ => x }


  // a single statement
  lazy val Stmt: Parser[Tokens, Stmt] =
    (p"skip").map[Stmt] { _ => Skip } ||
      (IdParser ~ p":=" ~ AExp).map[Stmt] { case x ~ _ ~ z => Assign(x, z) } ||
      (p"if" ~ BExp ~ p"then" ~ Block ~ p"else" ~ Block)
        .map[Stmt] { case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
      (p"while" ~ BExp ~ p"do" ~ Block).map[Stmt] { case _ ~ y ~ _ ~ w => While(y, w) } ||
      (p"(" ~ Stmt ~ p")").map[Stmt] { case _ ~ x ~ _ => x } ||
      ArrBlock.map(ArrVar)


  // statements
  lazy val Stmts: Parser[Tokens, Block] =
    (Stmt ~ SEMIParser ~ Stmts).map[Block] { case x ~ _ ~ z => x :: z } ||
      Stmt.map{ s => List(s) }


  // blocks (enclosed in curly braces)
  lazy val Block: Parser[Tokens, Block] =
    (p"{" ~ Stmts ~ p"}").map { case _ ~ y ~ _ => y } ||
      (p"(" ~ Stmts ~ p")").map { case _ ~ y ~ _ => y } ||
      Stmt.map(s => List(s))

  // helper function to parse programs (filters whitespases and comments)
  def parse(program: String) = {
    Stmts.parse_all(tokenise(program)).head
  }





  // ----------- Tokenizer ------------

  //  import $file.lexer
  //  import lexer._

  abstract class Token
  case object T_SEMI extends Token
  case object T_COMMA extends Token
  case object T_LPAREN extends Token
  case object T_RSQRB extends Token
  case object T_LSQRB extends Token
  case object T_RPAREN extends Token
  case object T_LBRACK extends Token
  case object T_RBRACK extends Token
  case class T_ID(s: String) extends Token
  case class T_OP(s: String) extends Token
  case class T_NUM(n: Int) extends Token
  case class T_KWD(s: String) extends Token
  case class T_STR(s: String) extends Token


  val token: PartialFunction[(String, String), Token] = {
    case ("semicolon", _) => T_SEMI
    case ("comma", _) => T_COMMA
    case ("parenthesis", "{") => T_LPAREN
    case ("parenthesis", "}") => T_RPAREN
    case ("brackets", "(") => T_LBRACK
    case ("brackets", ")") => T_RBRACK
    case ("sqr_brackets", "[") => T_LSQRB
    case ("sqr_brackets", "]") => T_RSQRB
    case ("id", s) => T_ID(s)
    case ("operation", s) => T_OP(s)
    case ("number", s) => T_NUM(s.toInt)
    case ("keyword", s) => T_KWD(s)
    case ("string", s) => T_STR(s)
  }

  // by using collect we filter out all unwanted tokens
  def tokenise(s: String): List[Token] =
    lexing_simp(RIMP_REGS, s).collect(token)





  // ------------ Interpreter -------------------

  //  import $file.parser
  //  import parser._

  // an interpreter for the WHILE language
  type Env = Map[String, Int]

  def eval_aexp(a: AExp, env: Env): Int = a match {
    case Num(i) => i
    case Var(s) => env(s)
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

  def eval_stmt(s: Stmt, env: Env): Env =
    s match {
      case Skip => env
      case Assign(x, a) => env + (x -> eval_aexp(a, env))
      //    case IArray(a) => env + Array[Int](eval_aexp(a, env))
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


  //  def question2() = {
  //    println("""Parsing "if (a < b) then skip else a := a * b + 1"""")
  //    println(parse("if (a < b) then skip else a := a * b + 1"))
  //    println("------------------------------------------------")
  //
  //    println("""Parsing "fibProg"""")
  //    println(parse(fibProg))
  //    println("------------------------------------------------")
  //
  //    println("""Parsing "loopProg"""")
  //    println(parse("start := 1000;" + loopProg))
  //    println("------------------------------------------------")
  //
  //    println("""Parsing "primesProg"""")
  //    println(parse(primesProg))
  //    println("------------------------------------------------")
  //
  //    println("""Parsing "collatzProg"""")
  //    println(parse(collatzProg))
  //    println("------------------------------------------------")
  //
  //  }
  //
  //
  //  def question3FibProg() = {
  //    println("""Evaluating "fibProg"""")
  //    println(eval(parse(fibProg)))
  //  }
  //
  //
  //  def question3LoopProg() = {
  //    for (i <- 100 to 1000 by 100) {
  //      println(s"""Evaluating "loopProg" with start := $i""")
  //      println(time(eval(parse(s"start := $i;" + loopProg))))
  //      println("------------------------------------------------")
  //    }
  //  }
  //
  //
  //  def question3PrimesProg() = {
  //    println("""Evaluating "primesProg"""")
  //    println(eval(parse(primesProg)))
  //  }
  //
  //
  //  def question3CollatzProg() = {
  //    println("""Evaluating "collatzProg"""")
  //    print(eval(parse(collatzProg)))
  //  }
  //
  //
  //  def testFact() = {
  //    println("""Evaluating "fact"""")
  //    val fact =
  //      """n := 4;
  //    result := 1;
  //     while (n > 0) do {
  //      result := n * result;
  //      n := n - 1
  //    };
  //    write result"""
  //    print(eval(parse(fact)))
  //  }


  //  val primesProg =
  //    """end := 100;
  //  n := 2;
  //  while (n < end) do {
  //    f := 2;
  //    tmp := 0;
  //    while ((f < n / 2 + 1) && (tmp == 0)) do {
  //      if ((n / f) * f == n) then  { tmp := 1 } else { skip };
  //      f := f + 1
  //    };
  //    if (tmp == 0) then { write(n) } else { skip };
  //    n  := n + 1
  //  }"""
  //
  //
  //  val fibProg =
  //    """write "Fib: ";
  //    read n;
  //    minus1 := 1;
  //    minus2 := 0;
  //    while n > 0 do {
  //    temp := minus2;
  //    minus2 := minus1 + minus2;
  //    minus1 := temp;
  //    n := n - 1
  //    };
  //    write "Result: ";
  //    write minus2 ;
  //    write "\n""""


  val exampleProg1 =
    """fact := 1;
    n := 3;
    while (!n > 0) do {
        fact := !n * !fact;
        n := !n - 1}
        """

  val exampleProg2 = """a := 49;
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


  def testLexer(): Unit = {
    println("# Lexing if (!a < !b) then skip else a := !a * !b + 1")
    println(lexing_simp(RIMP_REGS, "if (!a < !b) then skip else a := !a * !b + 1").filter(_._1 != "whitespace"))
    println("----------------------------------------------")

    println("# Lexing exampleProg1")
    println(lexing_simp(RIMP_REGS, exampleProg1).filter(_._1 != "whitespace"))
    println("----------------------------------------------")

    println("# Lexing exampleProg2")
    println(lexing_simp(RIMP_REGS, exampleProg2).filter(_._1 != "whitespace"))
    println("----------------------------------------------")

    println("# Lexing exampleProg3")
    println(lexing_simp(RIMP_REGS, exampleProg3).filter(_._1 != "whitespace"))
    println("----------------------------------------------")

    println("# Lexing exampleProg4")
    println(lexing_simp(RIMP_REGS, exampleProg4).filter(_._1 != "whitespace"))
    println("----------------------------------------------")

    println("# Lexing exampleProg5")
    println(lexing_simp(RIMP_REGS, exampleProg5).filter(_._1 != "whitespace"))
    println("----------------------------------------------")

  }

  def testParser(): Unit = {
    println("""Parsing "if (!a < !b) then skip else a := !a * !b + 1"""")
    println(parse("if (!a < !b) then skip else a := !a * !b + 1"))
    println("------------------------------------------------")

    println("Parsing exampleProg1")
    println(parse(exampleProg1))
    println("------------------------------------------------")

    println("Parsing exampleProg2")
    println(parse(exampleProg2))
    println("------------------------------------------------")

    println("Parsing exampleProg3")
    println(parse(exampleProg3))
    println("------------------------------------------------")

    println("Parsing exampleProg4")
    println(parse(exampleProg4))
    println("------------------------------------------------")

    println("Parsing exampleProg5")
    println(parse(exampleProg5))
    println("------------------------------------------------")

  }


  def testEvaluate(): Unit = {
    println("""Evaluating "exampleProg1"""")
    println(eval(parse(exampleProg5)))
    println("------------------------------------------------")
  }

}
