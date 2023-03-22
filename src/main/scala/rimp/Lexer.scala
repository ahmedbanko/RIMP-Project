package rimp

/**
 * Class Lexer.
 *
 * Lexer class includes implementation of functions performing
 * lexical analysis on a given plain text. The code used in this
 * class is developed from the source code given in Compiler and
 * Formal Languages Module at King's College London led by Dr. Christian Urban.
 */

class Lexer {
  //  --------- RIMP.Lexer -------------

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


  /** Some convenience for typing in regular expressions.
   *
   * @param s A list of Characters.
   * @return A regular expression matching the given character list.
   */
  def charlist2rexp(s: List[Char]): Rexp = s match {
    case Nil => ONE
    case c :: Nil => CHAR(c)
    case c :: s => SEQ(CHAR(c), charlist2rexp(s))
  }

  /**
   *  Convert a given string to a corresponding regular expression.
   * @param s A string value.
   * @return A regular expression matching the given string.
   */
  implicit def string2rexp(s: String): Rexp =
    charlist2rexp(s.toList)

  /**
   * Implicit conversion that allows Rexp objects to be used with infix operators.
   *
   * @param r The Rexp object to be converted.
   * @return An object with methods that allow the use of infix operators with regular expressions.
   */
  implicit def RexpOps(r: Rexp) = new {
    def |(s: Rexp) = ALT(r, s)
    def % = STAR(r)
    def ~(s: Rexp) = SEQ(r, s)
  }

  /**
   * Implicit conversion that allows String objects to be used with infix operators.
   *
   * @param s The String object being converted.
   * @return An object with methods that allow infix operators to be used with regular expressions.
   */
  implicit def stringOps(s: String) = new {
    def |(r: Rexp) = ALT(s, r)
    def |(r: String) = ALT(s, r)
    def % = STAR(s)
    def ~(r: Rexp) = SEQ(s, r)
    def $(r: Rexp) = RECD(s, r)
  }

  /**
   * Determines whether a regular expression can match the empty string.
   *
   * @param r The regular expression to check.
   * @return true if the regular expression can match the empty string, false otherwise.
   */
  def nullable(r: Rexp): Boolean = r match {
    case ZERO => false
    case ONE => true
    case CHAR(_) => false
    case ALT(r1, r2) => nullable(r1) || nullable(r2)
    case SEQ(r1, r2) => nullable(r1) && nullable(r2)
    case STAR(_) => true
    case RECD(_, r1) => nullable(r1)

    case NTIMES(r, n) => if (n == 0) true else nullable(r)
    case RANGE(_) => false
    case PLUS(r) => nullable(r)
    case OPTIONAL(_) => true
  }

  /**
   * Returns the derivative of the regular expression 'r' with respect to the character 'c'.
   *
   * @param c The character to differentiate with respect to.
   * @param r The regular expression to differentiate.
   * @return The derivative of the regular expression with respect to 'c'.
   */
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
  }


  /**
   * Flattens a given Val object into a string representation.
   *
   * @param v The Val object to flatten.
   * @return A string representation of the Val object.
   */
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


  /**
   * Returns the environment of a given value 'v', represented as a list of
   * pairs of strings.
   *
   * The environment of a value is the set of named variables that occur
   * within it, along with their corresponding values. The function recursively
   * traverses the structure of 'v' and collects all variable bindings into
   * a list of '(name, value)' pairs.
   *
   * @param v The value for which to compute the environment.
   * @return A list of '(name, value)' pairs representing the environment of 'v'.
   */
  def env(v: Val): List[(String, String)] = v match {
    case Empty => Nil
    case Chr(_) => Nil
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


  /**
   * Computes the empty derivative of a regular expression.
   *
   * @param r The regular expression to compute the empty derivative of.
   * @return A Val object representing the empty derivative of the regular expression.
   */
  def mkeps(r: Rexp): Val = r match {
    case ONE => Empty
    case ALT(r1, r2) =>
      if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
    case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
    case STAR(_) => Stars(Nil)
    case RECD(x, r) => Rec(x, mkeps(r))
    case PLUS(r) => Stars(List(mkeps(r)))
    case OPTIONAL(_) => Stars(Nil)
    case NTIMES(r, n) => Stars(List.fill(n)(mkeps(r)))
  }

  /**
   * Applies an injection function to a regular expression and a value.
   *
   * @param r The regular expression to apply the injection function to.
   * @param c The character to inject into the regular expression.
   * @param v The value to apply the injection function to.
   * @return The result of applying the injection function to the regular expression and value.
   */
  def inj(r: Rexp, c: Char, v: Val): Val = (r, v) match {
    case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1) :: vs)
    case (SEQ(r1, _), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
    case (SEQ(r1, _), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
    case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
    case (ALT(r1, _), Left(v1)) => Left(inj(r1, c, v1))
    case (ALT(_, r2), Right(v2)) => Right(inj(r2, c, v2))
    case (CHAR(_), Empty) => Chr(c)
    case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))

    case (RANGE(_), Empty) => Chr(c)
    case (PLUS(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1) :: vs)
    case (OPTIONAL(r), v) => Stars(inj(r, c, v) :: Nil)
    case (NTIMES(r, _), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1) :: vs)
  }


  // some "rectification" functions for simplification
  def F_ID(v: Val): Val = v
  def F_RIGHT(f: Val => Val): Val => Right = (v: Val) => Right(f(v))
  def F_LEFT(f: Val => Val): Val => Left = (v: Val) => Left(f(v))
  def F_ALT(f1: Val => Val, f2: Val => Val): Val => Val = (v: Val) => v match {
    case Right(v) => Right(f2(v))
    case Left(v) => Left(f1(v))
  }

  def F_SEQ(f1: Val => Val, f2: Val => Val): Val => Sequ = {
    case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
  }

  def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val): Val => Sequ =
    (v: Val) => Sequ(f1(Empty), f2(v))

  def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val): Val => Sequ =
    (v: Val) => Sequ(f1(v), f2(Empty))

  def F_ERROR(v: Val): Val = throw new Exception("error")

  /**
   * Simplifies a regular expression and returns an equivalent
   * regular expression and a corresponding function that transforms the matches of the
   * simplified expression into matches of the original expression.
   *
   * @param r The regular expression to simplify.
   * @return A tuple containing the simplified regular expression and a transformation function.
   */
  def simp(r: Rexp): (Rexp, Val => Val) = r match {
    case ALT(r1, r2) =>
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (ZERO, _) => (r2s, F_RIGHT(f2s))
        case (_, ZERO) => (r1s, F_LEFT(f1s))
        case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
        else (ALT(r1s, r2s), F_ALT(f1s, f2s))
      }
    case SEQ(r1, r2) =>
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (ZERO, _) => (ZERO, F_ERROR)
        case (_, ZERO) => (ZERO, F_ERROR)
        case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
        case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
        case _ => (SEQ(r1s, r2s), F_SEQ(f1s, f2s))
      }
    case r => (r, F_ID)
  }

  /**
   * Applies lexical analysis to the input string 's' using the regular expression 'r'.
   *
   * @param r The regular expression to be used for lexical analysis.
   * @param s The input string to be analyzed.
   * @return A 'Val' object representing the result of the analysis.
   * @throws Exception if the input string does not match the regular expression.
   */
  def lex_simp(r: Rexp, s: List[Char]): Val = s match {
    case Nil => if (nullable(r)) mkeps(r) else {
      throw new Exception("lexing error")
    }
    case c :: cs =>
      val (r_simp, f_simp) = simp(der(c, r))
      inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }

  /**
   * Performs simple lexing of a given regular expression 'r' on a given input string 's',
   * and returns a list of pairs, where each pair consists of a token and its corresponding lexeme.
   *
   * @param r The regular expression to use for lexing.
   * @param s The input string to lex.
   * @return A list of pairs, where each pair consists of a token and its corresponding string expression.
   */
  def lexing_simp(r: Rexp, s: String): List[(String, String)] =
    env(lex_simp(r, s.toList))


  /**
   * RIMP grammar represented as regular expressions.
   */
  val KEYWORD: Rexp = "skip" | "while" | "do" | "if" | "then" | "else"
  val OP: Rexp = "+" | "-" | "*" | "%" | "/" | "=" | "!=" | ">" | "<" | "<=" | ">=" | ":=" | "!" | "~"
  val alphabet: String = ('A' to 'Z').toList.mkString ++ ('a' to 'z').toList.mkString
  val LETTER: Rexp = RANGE(alphabet.toSet)
  val WHITESPACE: Rexp = PLUS(" " | "\n" | "\t" | "\r")
  val DIGIT: Rexp = RANGE("0123456789".toSet)
  val RSQRB: Rexp = "]"
  val LSQRB: Rexp = "["
  val RPAREN: Rexp = "}"
  val LPAREN: Rexp = "{"
  val RBRACK: Rexp = ")"
  val LBRACK: Rexp = "("
  val BAR: Rexp = "|"
  val SEMI: Rexp = ";"
  val COMMA: Rexp = ","
  val ID: Rexp = LETTER ~ ("_" | LETTER | DIGIT).%
  val NUM: Rexp = "0" | OPTIONAL(CHAR('-')) ~ (RANGE("123456789".toSet) ~ STAR(DIGIT))

  val RIMP_REGS: STAR = (("keyword" $ KEYWORD) |
    ("id" $ ID) |
    ("operation" $ OP) |
    ("number" $ NUM) |
    ("semicolon" $ SEMI) |
    ("comma" $ COMMA) |
    ("bar" $ BAR) |
    ("parenthesis" $ (LPAREN | RPAREN)) |
    ("brackets" $ (RBRACK | LBRACK)) |
    ("sqr_brackets" $ (RSQRB | LSQRB)) |
    ("whitespace" $ WHITESPACE) |
    ("digit" $ DIGIT)).%


}
