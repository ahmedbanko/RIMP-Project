package rimp

import scala.collection.mutable

class Parser extends Tokenizer {
  //  --------- RIMP.Parser -------------


  case class ~[+A, +B](x: A, y: B)

  // constraint for the input
  type IsSeq[A] = A => Seq[_]

  type Tokens = Seq[Token]
  type RArray = mutable.Stack[Array[RVar]]

  var while_count: Int = 0
  var if_count: Int = 0

  def whileID(): String = {
    while_count += 1
    s"_k$while_count"
  }

  def ifID(): String = {
    if_count += 1
    s"_if$if_count"
  }


  case class Counter(id: String="NONE", count: Int = 0)

  abstract class Parser[I: IsSeq, T] {
    def parse(in: I): Set[(T, I)]

    def parse_all(in: I): Set[T] =
      for ((hd, tl) <- parse(in)
           if tl.isEmpty) yield hd
  }

  // parser combinators

  // sequence parser
  class SeqParser[I: IsSeq, T, S](p: => Parser[I, T],
                                  q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
    def parse(in: I): Set[(T ~ S, I)] =
      for ((hd1, tl1) <- p.parse(in);
           (hd2, tl2) <- q.parse(tl1)) yield (new ~(hd1, hd2), tl2)
  }

  // alternative parser
  class AltParser[I: IsSeq, T](p: => Parser[I, T],
                               q: => Parser[I, T]) extends Parser[I, T] {
    def parse(in: I): Set[(T, I)] = p.parse(in) ++ q.parse(in)
  }

  // map parser
  class MapParser[I: IsSeq, T, S](p: => Parser[I, T],
                                  f: T => S) extends Parser[I, S] {
    def parse(in: I): Set[(S, I)] = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
  }

  case class TokenParser(sin: String) extends Parser[Tokens, Token] {
    def parse(in: Tokens): Set[(Token, Tokens)] = in match {
      case T_LBRACK :: tail if sin == "(" => Set((T_LBRACK, tail))
      case T_RBRACK :: tail if sin == ")" => Set((T_RBRACK, tail))
      case T_LSQRB :: tail if sin == "[" => Set((T_LSQRB, tail))
      case T_RSQRB :: tail if sin == "]" => Set((T_RSQRB, tail))
      case T_LPAREN :: tail if sin == "{" => Set((T_LPAREN, tail))
      case T_RPAREN :: tail if sin == "}" => Set((T_RPAREN, tail))
      case T_ID(s) :: tail if s == sin => Set((T_ID(s), tail))
      case T_OP(s) :: tail if s == sin => Set((T_OP(s), tail))
      case T_NUM(n) :: tail if n.toString == sin => Set((T_NUM(n), tail))
      case T_KWD(s) :: tail if s == sin => Set((T_KWD(s), tail))
      case _ => Set()
    }
  }

  case object CommaParser extends Parser[Tokens, String] {
    def parse(in: Tokens): Set[(String, Tokens)] = in match {
      case T_COMMA :: tail => Set((",", tail))
      case _ => Set()
    }
  }

  case object SEMIParser extends Parser[Tokens, String] {
    def parse(in: Tokens): Set[(String, Tokens)] = in match {
      case T_SEMI :: tail => Set((";", tail))
      case _ => Set()
    }
  }

  case object BarParser extends Parser[Tokens, String] {
    def parse(in: Tokens): Set[(String, Tokens)] = in match {
      case T_BAR :: tail => Set(("|", tail))
      case _ => Set()
    }
  }


  case object IdParser extends Parser[Tokens, String] {
    def parse(in: Tokens): Set[(String, Tokens)] = in match {
      case T_ID(s) :: tail => Set((s, tail))
      case _ => Set()
    }
  }


  case object NumParser extends Parser[Tokens, Int] {
    def parse(in: Tokens): Set[(Int, Tokens)] = in match {
      case T_NUM(n) :: tail => Set((n, tail))
      case _ => Set()
    }
  }


  implicit def parser_interpolation(sc: StringContext) = new {
    def p(args: Any*): TokenParser = TokenParser(sc.s(args: _*))
  }

  // more convenient syntax for parser combinators
  implicit def ParserOps[I: IsSeq, T](p: Parser[I, T]) = new {
    def ||(q: => Parser[I, T]) = new AltParser[I, T](p, q)

    def ~[S](q: => Parser[I, S]) = new SeqParser[I, T, S](p, q)

    def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
  }

  abstract class Exp
  abstract class Stmt extends Exp
  abstract class AExp extends Exp
  abstract class BExp extends Exp


  type Block = List[Stmt]
  type ArrBlock = Array[AExp]

  case object Skip extends Stmt
  case class If(a: BExp, bl1: Block, bl2: Block, id: String="NONE") extends Stmt
  case class While(b: BExp, bl: Block, counter: Counter) extends Stmt
  case class Assign(s: String, a: AExp) extends Stmt
  case class AssignArr(id: String, values: Array[AExp]) extends Stmt
  case class ArrayWithSize(id: String, size: AExp) extends Stmt
  case class UpdateArrIndex(id: String, index: AExp, newVal: AExp) extends Stmt

  case class Var(s: String) extends AExp
  case class ArrayVar(id: String, index: AExp) extends AExp
  case class Num(i: Int) extends AExp
  case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

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
      (p"!" ~ IdParser).map[AExp]{ case _ ~ x => Var(x) } ||
      NumParser.map(Num)

  lazy val ArrBlock: Parser[Tokens, ArrBlock] =
    (p"[" ~ ArrVals ~ p"]").map { case _ ~ y ~ _ => y }


  lazy val ArrVals: Parser[Tokens, ArrBlock] =
    (AExp ~ CommaParser ~ ArrVals).map[ArrBlock] { case x ~ _ ~ z => x +: z } ||
      AExp.map(x => Array(x))


  // boolean expressions with some simple nesting
  lazy val BExp: Parser[Tokens, BExp] =
    (AExp ~ p"=" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("=", x, z) } ||
      (AExp ~ p"!=" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("!=", x, z) } ||
      (AExp ~ p"<" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("<", x, z) } ||
      (AExp ~ p">" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop(">", x, z) } ||
      (AExp ~ p">=" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop(">=", x, z) } ||
      (AExp ~ p"<=" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("<=", x, z) } ||
      (p"~" ~ BExp).map[BExp] { case _ ~ x => Not(x) } ||
      (p"(" ~ BExp ~ p")").map[BExp] { case _ ~ x ~ _ => x }


  // a single statement
  lazy val Stmt: Parser[Tokens, Stmt] =
    p"skip".map[Stmt] { _ => Skip } ||
      (IdParser ~ p":=" ~ AExp).map[Stmt] { case x ~ _ ~ z => Assign(x, z) } ||
      (IdParser ~ p":=" ~ ArrBlock).map[Stmt] { case id ~ _ ~ values => AssignArr(id, values) } ||
      (IdParser ~ p":=" ~ BarParser ~ AExp ~ BarParser).map[Stmt] {
        case id ~ _ ~ _ ~ size ~ _  => ArrayWithSize(id, size)} ||
      (IdParser ~ p"[" ~ AExp ~ p"]" ~ p":=" ~ AExp).map[Stmt] {
        case id ~ _ ~ index ~ _ ~ _ ~ newVal => UpdateArrIndex(id, index, newVal)
      } ||
      (p"if" ~ BExp ~ p"then" ~ Block ~ p"else" ~ Block)
        .map[Stmt] { case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
      (p"while" ~ BExp ~ p"do" ~ Block).map[Stmt] { case _ ~ y ~ _ ~ w => While(y, w, Counter())} ||
      (p"(" ~ Stmt ~ p")").map[Stmt] { case _ ~ x ~ _ => x }

  // statements
  lazy val Stmts: Parser[Tokens, Block] =
    (Stmt ~ SEMIParser ~ Stmts).map[Block] { case x ~ _ ~ z => x :: z } ||
      Stmt.map { s => List(s) }


  // blocks (enclosed in curly braces)
  lazy val Block: Parser[Tokens, Block] =
    (p"{" ~ Stmts ~ p"}").map { case _ ~ y ~ _ => y } ||
      Stmt.map(s => List(s))

  def injectIds(b: Block): List[Stmt] = {
    b.map {
      case w: While => w.copy(b = w.b, counter = Counter(id = whileID()), bl = injectIds(w.bl))
      case i: If => i.copy(a = i.a, id = ifID(), bl1 = injectIds(i.bl1), bl2 = injectIds(i.bl2))
      case other => other
    }
  }

  // helper function to parse programs (filters whitespaces and comments)
  def parse(program: String): List[Stmt]  = {
    val p = Stmts.parse_all(tokenize(program)).head
    injectIds(p)
  }

  private def stmt2String(stmt: Exp): String = stmt match {
    case Skip => "skip"
    case If(a, bl1, bl2, if_id) =>
      val ifId = if_id.tail(2)
      s"$if_id := 0;\nif-$ifId ${stmt2String(a)} then {\n$if_id := 1;\n${bl1.map(x => stmt2String(x)).mkString(";\n")}\n} else {\n$if_id := 0;\n${bl2.map(x => stmt2String(x)).mkString(";\n")}\n}"
    case While(b, bl, counter) =>
      val whileId = counter.id.tail.tail
      s"${counter.id} := ${counter.count};\nwhile-$whileId ${stmt2String(b)} do {\n${bl.map(x => stmt2String(x)).mkString(";\n")};\n${counter.id} := !${counter.id} + 1\n}"
    case Assign(s, a) => s"$s := ${stmt2String(a)}"
    case AssignArr(id, values) => s"$id := ${values.map(stmt2String).mkString("[", ", ", "]")}"
    case ArrayWithSize(id, size) => s"$id := |${stmt2String(size)}|"
    case UpdateArrIndex(id, index, newVal) => s"$id[${stmt2String(index)}] := ${stmt2String(newVal)}"
    case ArrayVar(id, index) => s"$id[${stmt2String(index)}]"
    case Var(s) => s"!$s"
    case Num(i) => s"$i"
    case Aop(o, a1, a2) => s"${stmt2String(a1)} $o ${stmt2String(a2)}"
    case Bop(o, a1, a2) => s"(${stmt2String(a1)} $o ${stmt2String(a2)})"
    case Not(b) => s"~${stmt2String(b)}"
  }

  private def stmts2String(ast: List[Stmt], output: List[String] = List()) : List[String] = ast match {
    case Nil => output
    case s::rest =>
      if(rest.isEmpty) {
        stmt2String(s)::stmts2String(rest)
      }else {
        s"${stmt2String(s)};\n"::stmts2String(rest)
      }
  }

  private def stmt2RevStr(stmt: Exp): String = stmt match {
    case Skip => "skip"
    case If(_, bl1, bl2, if_id) =>
      val ifId = if_id.tail(2)
      s"if-$ifId (!$if_id = 1) then {\n$if_id =: 1;\n${bl1.reverse.map(x => stmt2RevStr(x)).mkString(";\n")}\n} else {\n$if_id =: 0;\n${bl2.reverse.map(x => stmt2RevStr(x)).mkString(";\n")}\n}"
    case While(_, bl, counter) =>
      val whileId = counter.id.tail.tail
      s"while-$whileId (!${counter.id} > 0) do {\n${counter.id} =: !${counter.id} + 1;\n${bl.reverse.map(x => stmt2RevStr(x)).mkString(";\n")}\n};\n${counter.id} =: 0"
    case Assign(s, a) => s"$s =: ${stmt2RevStr(a)}"
    case AssignArr(id, values) => s"$id =: ${values.mkString("[", ", ", "]")}"
    case ArrayWithSize(id, size) => s"$id =: |${stmt2RevStr(size)}|"
    case UpdateArrIndex(id, index, newVal) => s"$id[${stmt2RevStr(index)}] =: ${stmt2RevStr(newVal)}"
    case ArrayVar(id, index) => s"$id[${stmt2RevStr(index)}]"
    case Var(s) => s"!$s"
    case Num(i) => s"$i"
    case Aop(o, a1, a2) => s"${stmt2RevStr(a1)} $o ${stmt2RevStr(a2)}"
    case Bop(o, a1, a2) => s"(${stmt2RevStr(a1)} $o ${stmt2RevStr(a2)})"
    case Not(b) => s"~${stmt2RevStr(b)}"
  }

  private def stmts2RevStr(ast: List[Stmt], output: List[String] = List()): List[String] = ast match {
    case Nil => output
    case s :: rest =>
      if (rest.isEmpty) {
        stmt2RevStr(s) :: stmts2RevStr(rest)
      } else {
        s"${stmt2RevStr(s)};\n" :: stmts2RevStr(rest)
      }
  }

  private def revStmt(stmt: Stmt) = stmt match {
    case If (_, bl1, bl2, id) =>
      If(Bop("=", Var(id), Num(1)), revAST(bl1), revAST(bl2), id)
    case While(_, bl, counter) =>
      While(Bop(">", Var(counter.id), Num(0)), revAST(bl), counter)
    case _ => stmt
  }

  def revAST(stmts: List[Stmt], output: List[Stmt] = List()): List[Stmt] = stmts match {
    case Nil => output
    case hd::tail => revAST(tail, output):+revStmt(hd)
  }

  def translateRev(code: String) : String = {
    val ast = parse(code)
    stmts2RevStr(ast.reverse).mkString.split("\n").filterNot(_.isEmpty).mkString("\n")
  }

  def translate(code: String): String = {
    val ast = parse(code)
    stmts2String(ast).mkString.split("\n").filterNot(_.isEmpty).mkString("\n")
  }


  def stack(vars: Int*): RVar = {
    val out = new RVar()
    vars.foreach(x => out.push(x))
    out
  }

  def resetCounters: Unit = {
     while_count = 0
     if_count = 0
  }
}
