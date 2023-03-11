package rimp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class ParserTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach {
  val p = new Parser
  val fixtures = new Fixtures

  override protected def afterEach(): Unit = {
    super.afterEach()
   
  }

  val ex1_translated = "fact := 1;\nn := 3;\n_k1 := 0;\nwhile-1 (!n > 0) do {\nfact := !n * !fact;\nn := !n - 1;\n_k1 := !_k1 + 1\n}"
  val ex2_translated = "a := 49;\nb := 28;\n_k1 := 0;\nwhile-1 ~(!a = !b) do {\n_if1 := 0;\nif-1 (!a > !b) then {\na := !a - !b;\n_if1 := 1\n} else {\nb := !b - !a;\n_if1 := 0\n};\n_k1 := !_k1 + 1\n}"
  val ex3_translated = "x := 12;\n_k1 := 0;\nwhile-1 (!x > 1) do {\nr := !x;\n_k2 := 0;\nwhile-2 (!r > 1) do {\nr := !r - 2;\n_k2 := !_k2 + 1\n};\n_if1 := 0;\nif-1 (!r = 0) then {\n_if1 := 1;\nx := !x / 2\n} else {\n_if1 := 0;\nx := 3 * !x + 1\n};\n_k1 := !_k1 + 1\n}"
  val ex4_translated = "x := 13;\nfactor := 2;\nisprime := 1;\nlimit := !x / 2 + 1;\n_k1 := 0;\nwhile-1 (!factor < !limit) do {\nr := !x;\n_k2 := 0;\nwhile-2 (!r > !factor - 1) do {\nr := !r - !factor;\n_k2 := !_k2 + 1\n};\n_if1 := 0;\nif-1 (!r = 0) then {\n_if1 := 1;\nisprime := 0\n} else {\n_if1 := 0;\nskip\n};\nfactor := !factor + 1;\n_k1 := !_k1 + 1\n}"
  val ex5_translated = "n := 10;\na := 1;\nb := 1;\ni := 0;\n_k1 := 0;\nwhile-1 (!i < !n) do {\ntmp := !a;\na := !b;\nb := !tmp + !a;\ni := !i + 1;\n_k1 := !_k1 + 1\n}"
  val arrProg1_translated = "arr := |10|;\ni := 0;\n_k1 := 0;\nwhile-1 (!i < 10) do {\narr[!i] := !i;\ni := !i + 1;\n_k1 := !_k1 + 1\n};\nii := 0;\n_k2 := 0;\nwhile-2 (!ii < 10) do {\nx := arr[!ii];\nii := !ii + 1;\n_k2 := !_k2 + 1\n}"
  val reverse_arr_prog_translated = "arr := [1, 2, 3, 4, 5];\narr_len := 5;\nleft := 0;\nright := !arr_len - 1;\n_k1 := 0;\nwhile-1 (!left < !right) do {\ntmp_left := arr[!left];\ntmp_right := arr[!right];\narr[!right] := !tmp_left;\narr[!left] := !tmp_right;\nleft := !left + 1;\nright := !right - 1;\n_k1 := !_k1 + 1\n}"


  test("Test translate function") {
    assert(p.translate(fixtures.EX1) == ex1_translated)
    assert(p.translate(fixtures.EX2) == ex2_translated)
    assert(p.translate(fixtures.EX3) == ex3_translated)
    assert(p.translate(fixtures.EX4) == ex4_translated)
    assert(p.translate(fixtures.EX5) == ex5_translated)
    assert(p.translate(fixtures.arrProg1) == arrProg1_translated)
    assert(p.translate(fixtures.reverse_arr_prog) == reverse_arr_prog_translated)

  }

  val ex1_inverted = "while-1 (!_k1 > 0) do {\n_k1 =: !_k1 + 1;\nn =: !n - 1;\nfact =: !n * !fact\n};\n_k1 =: 0;\nn =: 3;\nfact =: 1"
  val ex2_inverted = "while-1 (!_k1 > 0) do {\n_k1 =: !_k1 + 1;\nif-1 (!_if1 = 1) then {\n_if1 =: 1;\na =: !a - !b\n} else {\n_if1 =: 0;\nb =: !b - !a\n}\n};\n_k1 =: 0;\nb =: 28;\na =: 49"
  val ex3_inverted = "while-1 (!_k1 > 0) do {\n_k1 =: !_k1 + 1;\nif-1 (!_if1 = 1) then {\n_if1 =: 1;\nx =: !x / 2\n} else {\n_if1 =: 0;\nx =: 3 * !x + 1\n};\nwhile-2 (!_k2 > 0) do {\n_k2 =: !_k2 + 1;\nr =: !r - 2\n};\n_k2 =: 0;\nr =: !x\n};\n_k1 =: 0;\nx =: 12"
  val ex4_inverted = "while-1 (!_k1 > 0) do {\n_k1 =: !_k1 + 1;\nfactor =: !factor + 1;\nif-1 (!_if1 = 1) then {\n_if1 =: 1;\nisprime =: 0\n} else {\n_if1 =: 0;\nskip\n};\nwhile-2 (!_k2 > 0) do {\n_k2 =: !_k2 + 1;\nr =: !r - !factor\n};\n_k2 =: 0;\nr =: !x\n};\n_k1 =: 0;\nlimit =: !x / 2 + 1;\nisprime =: 1;\nfactor =: 2;\nx =: 13"
  val ex5_inverted = "while-1 (!_k1 > 0) do {\n_k1 =: !_k1 + 1;\ni =: !i + 1;\nb =: !tmp + !a;\na =: !b;\ntmp =: !a\n};\n_k1 =: 0;\ni =: 0;\nb =: 1;\na =: 1;\nn =: 10"
  val arrProg1_inverted = "while-2 (!_k2 > 0) do {\n_k2 =: !_k2 + 1;\nii =: !ii + 1;\nx =: arr[!ii]\n};\n_k2 =: 0;\nii =: 0;\nwhile-1 (!_k1 > 0) do {\n_k1 =: !_k1 + 1;\ni =: !i + 1;\narr[!i] =: !i\n};\n_k1 =: 0;\ni =: 0;\narr =: |10|"
  val reverse_arr_prog_inverted= "while-1 (!_k1 > 0) do {\n_k1 =: !_k1 + 1;\nright =: !right - 1;\nleft =: !left + 1;\narr[!left] =: !tmp_right;\narr[!right] =: !tmp_left;\ntmp_right =: arr[!right];\ntmp_left =: arr[!left]\n};\n_k1 =: 0;\nright =: !arr_len - 1;\nleft =: 0;\narr_len =: 5;\narr =: [1, 2, 3, 4, 5]"

  test("Test translateRev function") {
    assert(p.invert(fixtures.EX1) == ex1_inverted)
    assert(p.invert(fixtures.EX2) == ex2_inverted)
    assert(p.invert(fixtures.EX3) == ex3_inverted)
    assert(p.invert(fixtures.EX4) == ex4_inverted)
    assert(p.invert(fixtures.EX5) == ex5_inverted)
    assert(p.invert(fixtures.arrProg1) == arrProg1_inverted)
    assert(p.invert(fixtures.reverse_arr_prog) == reverse_arr_prog_inverted)
  }


}
