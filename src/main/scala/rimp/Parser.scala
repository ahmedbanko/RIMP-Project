package rimp

class Parser extends Tokenizer {
  //  --------- RIMP.Parser -------------

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


  type Block = List[Stmt]
  type ArrBlock = List[AExp]

  case object Skip extends Stmt

  case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt

  case class While(b: BExp, bl: Block) extends Stmt

  case class Assign(s: String, a: AExp) extends Stmt

  case class AssignArr(id: String, values: List[AExp]) extends Stmt

  case class UpdateArrIndex(id: String, index: AExp, newVal: AExp) extends Stmt

  case class ArrayVar(id: String, index: AExp) extends AExp

  case class AssignThread(id: String, bl: Block) extends Stmt
  case class RunThread(id: String) extends Stmt

  case class Var(s: String) extends AExp

  case class Num(i: Int) extends AExp

  case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

  case class WriteStr(s: String) extends Stmt
  case class WriteVar(s: String) extends Stmt
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
      (IdParser ~ p"[" ~ AExp ~ p"]").map { case id ~ _ ~ index ~ _ => ArrayVar(id, index) } ||
      (p"!" ~ IdParser).map { case _ ~ x => Var(x) } ||
      NumParser.map(Num)

  lazy val ArrBlock: Parser[Tokens, ArrBlock] =
    (p"[" ~ ArrVals ~ p"]").map { case _ ~ y ~ _ => y } ||
      (p"[" ~ p"]").map { case _ ~ _ => List() }


  lazy val ArrVals: Parser[Tokens, ArrBlock] =
    (AExp ~ CommaParser ~ ArrVals).map[ArrBlock] { case x ~ _ ~ z => x :: z } ||
      AExp.map(x => List(x))


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
      (p"~" ~ BExp).map[BExp] { case _ ~ x => Not(x) } ||
      (p"(" ~ BExp ~ p")").map[BExp] { case _ ~ x ~ _ => x }


  // a single statement
  lazy val Stmt: Parser[Tokens, Stmt] =
    (p"skip").map[Stmt] { _ => Skip } ||
      (IdParser ~ p":=" ~ AExp).map[Stmt] { case x ~ _ ~ z => Assign(x, z) } ||
      (p"write" ~ StrParser).map[Stmt] { case _ ~ y => WriteStr(y) } ||
      (p"write" ~ p"!" ~ IdParser).map[Stmt] { case _ ~ _ ~ y => WriteVar(y) } ||
      (IdParser ~ p":=" ~ ArrBlock).map { case id ~ _ ~ values => AssignArr(id, values) } ||
      (IdParser ~ p"[" ~ AExp ~ p"]" ~ p":=" ~ AExp).map {
        case id ~ _ ~ index ~ _ ~ _ ~ newVal => UpdateArrIndex(id, index, newVal)
      } ||
      (p"if" ~ BExp ~ p"then" ~ Block ~ p"else" ~ Block)
        .map[Stmt] { case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
      (p"while" ~ BExp ~ p"do" ~ Block).map[Stmt] { case _ ~ y ~ _ ~ w => While(y, w) } ||
      (p"thread" ~ IdParser ~ p":=" ~ Block ).map[Stmt] { case _ ~ id ~ _ ~ bl => AssignThread(id, bl) } ||
      (p"run" ~ p"?" ~ IdParser).map[Stmt] { case _ ~ _ ~ id  => RunThread(id) } ||
      (p"(" ~ Stmt ~ p")").map[Stmt] { case _ ~ x ~ _ => x }

  //  ArrBlock.map(ArrVal)


  // statements
  lazy val Stmts: Parser[Tokens, Block] =
    (Stmt ~ SEMIParser ~ Stmts).map[Block] { case x ~ _ ~ z => x :: z } ||
      Stmt.map { s => List(s) }


  // blocks (enclosed in curly braces)
  lazy val Block: Parser[Tokens, Block] =
    (p"{" ~ Stmts ~ p"}").map { case _ ~ y ~ _ => y } ||
      Stmt.map(s => List(s))

  // helper function to parse programs (filters whitespases and comments)
  def parse(program: String) = {
    Stmts.parse_all(tokenize(program)).head
  }
}
