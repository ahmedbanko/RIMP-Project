package rimp

import org.scalatest.funsuite.AnyFunSuite

class LexerTest extends AnyFunSuite {
  val lex =  new Lexer()
  test("Test charlist2rexp function") {
    val listIn = "hello world".toList
    val expected = lex.SEQ(lex.CHAR('h'), lex.SEQ(lex.CHAR('e'), lex.SEQ(lex.CHAR('l'),
      lex.SEQ(lex.CHAR('l'), lex.SEQ(lex.CHAR('o'), lex.SEQ(lex.CHAR(' '), lex.SEQ(lex.CHAR('w'),
        lex.SEQ(lex.CHAR('o'), lex.SEQ(lex.CHAR('r'), lex.SEQ(lex.CHAR('l'), lex.CHAR('d')))))))))))
    assert(lex.charlist2rexp(listIn) == expected)
    assert(lex.charlist2rexp(List()) == lex.ONE)
  }

  test("Test string2rexp implicit function") {
    val expected = lex.SEQ(lex.CHAR('h'), lex.SEQ(lex.CHAR('e'), lex.SEQ(lex.CHAR('l'),
      lex.SEQ(lex.CHAR('l'), lex.SEQ(lex.CHAR('o'), lex.SEQ(lex.CHAR(' '), lex.SEQ(lex.CHAR('w'),
        lex.SEQ(lex.CHAR('o'), lex.SEQ(lex.CHAR('r'), lex.SEQ(lex.CHAR('l'), lex.CHAR('d')))))))))))
    assert(lex.string2rexp("hello world") == expected)
  }

  test("Test RexpOps implicit function") {
    assert(lex.RexpOps(lex.ONE).~(lex.ZERO) == lex.SEQ(lex.ONE, lex.ZERO))
    assert(lex.RexpOps(lex.RANGE(Set('a', 'z'))).|(lex.RECD("semi", lex.CHAR(';'))) == lex.ALT(lex.RANGE(Set('a', 'z')), lex.RECD("semi", lex.CHAR(';'))))
    assert(lex.RexpOps(lex.NTIMES(lex.OPTIONAL(lex.PLUS(lex.CHAR('c'))), 5)).% == lex.STAR(lex.NTIMES(lex.OPTIONAL(lex.PLUS(lex.CHAR('c'))), 5)))
  }

  test("Test stringOps implicit function") {
    assert(lex.stringOps("a").~(lex.ZERO) == lex.SEQ(lex.CHAR('a'), lex.ZERO))
    assert(lex.stringOps("b").|(lex.RECD("semi", lex.CHAR(';'))) == lex.ALT(lex.CHAR('b'), lex.RECD("semi", lex.CHAR(';'))))
    assert(lex.stringOps("c").|("hello") == lex.ALT(lex.CHAR('c'), lex.SEQ(lex.CHAR('h'), lex.SEQ(lex.CHAR('e'), lex.SEQ(lex.CHAR('l'),
      lex.SEQ(lex.CHAR('l'), lex.CHAR('o')))))))
    assert(lex.stringOps("d").% == lex.STAR(lex.CHAR('d')))
    assert(lex.stringOps("comma").$(lex.CHAR(',')) == lex.RECD("comma", lex.CHAR(',')))
  }


  test("Test nullable function") {
    assert(!lex.nullable(lex.ZERO))
    assert(lex.nullable(lex.ONE))
    assert(!lex.nullable(lex.CHAR('x')))
    assert(!lex.nullable(lex.ALT(lex.CHAR('x'), lex.CHAR('x'))))
    assert(lex.nullable(lex.ALT(lex.ONE, lex.CHAR('x'))))
    assert(lex.nullable(lex.ALT(lex.CHAR('x'), lex.ONE)))
    assert(lex.nullable(lex.ALT(lex.ONE, lex.STAR(lex.CHAR('x')))))
    assert(!lex.nullable(lex.SEQ(lex.CHAR('x'), lex.CHAR('x'))))
    assert(!lex.nullable(lex.SEQ(lex.ONE, lex.CHAR('x'))))
    assert(!lex.nullable(lex.SEQ(lex.CHAR('x'), lex.ONE)))
    assert(lex.nullable(lex.SEQ(lex.ONE, lex.STAR(lex.CHAR('x')))))
    assert(lex.nullable(lex.STAR(lex.ONE)))
    assert(lex.nullable(lex.STAR(lex.CHAR('x'))))
    assert(!lex.nullable(lex.RECD("comma", lex.CHAR(','))))
    assert(lex.nullable(lex.RECD("star", lex.STAR(lex.CHAR(',')))))
    assert(lex.nullable(lex.NTIMES(lex.CHAR('x'), 0)))
    assert(lex.nullable(lex.NTIMES(lex.STAR(lex.CHAR('x')), 2)))
    assert(!lex.nullable(lex.NTIMES(lex.CHAR('x'), 2)))
    assert(!lex.nullable(lex.RANGE(Set('a', 'z'))))
    assert(!lex.nullable(lex.PLUS(lex.RANGE(Set('a', 'z')))))
    assert(lex.nullable(lex.PLUS(lex.STAR(lex.CHAR('x')))))
    assert(lex.nullable(lex.OPTIONAL(lex.STAR(lex.CHAR('x')))))
    assert(lex.nullable(lex.OPTIONAL(lex.CHAR('x'))))
  }

  test("Test der function") {
    assert(lex.der('c', lex.ZERO) == lex.ZERO)
    assert(lex.der('c', lex.ONE) == lex.ZERO)
    assert(lex.der('c', lex.CHAR('d')) == lex.ZERO)
    assert(lex.der('d', lex.CHAR('c')) == lex.ZERO)
    assert(lex.der('d', lex.CHAR('d')) == lex.ONE)
    assert(lex.der('d', lex.ALT(lex.CHAR('d'), lex.CHAR('d'))) == lex.ALT(lex.ONE, lex.ONE))
    assert(lex.der('c', lex.ALT(lex.CHAR('c'), lex.CHAR('d'))) == lex.ALT(lex.ONE, lex.ZERO))
    assert(lex.der('d', lex.SEQ(lex.ONE, lex.CHAR('d'))) == lex.ALT(lex.SEQ(lex.ZERO, lex.CHAR('d')), lex.ONE))
    assert(lex.der('d', lex.SEQ(lex.CHAR('d'), lex.CHAR('d'))) == lex.SEQ(lex.ONE, lex.CHAR('d')))
    assert(lex.der('d', lex.STAR(lex.CHAR('d'))) == lex.SEQ(lex.ONE, lex.STAR(lex.CHAR('d'))))
    assert(lex.der(',', lex.RECD("comma", lex.CHAR(','))) == lex.ONE)
    assert(lex.der(',', lex.NTIMES(lex.CHAR('c'), 0)) == lex.ZERO)
    assert(lex.der(',', lex.NTIMES(lex.CHAR('c'), 4)) == lex.SEQ(lex.ZERO, lex.NTIMES(lex.CHAR('c'), 3)))
    assert(lex.der('a', lex.RANGE("abcdefg".toSet)) == lex.ONE)
    assert(lex.der('z', lex.RANGE("abcdefg".toSet)) == lex.ZERO)
    assert(lex.der('c', lex.PLUS(lex.CHAR('c'))) == lex.SEQ(lex.ONE, lex.STAR(lex.CHAR('c'))))
    assert(lex.der('c', lex.OPTIONAL(lex.CHAR('c'))) == lex.ONE)
    assert(lex.der('d', lex.OPTIONAL(lex.CHAR('c'))) == lex.ZERO)
  }
}
