package rimp

class Fixtures {

  val EX1 =
    """
       fact := 1;
       n := 3;
       while (!n > 0) do {
            fact := !n * !fact;
            n := !n - 1
       }
      """

  val EX2 =
    """a := 49;
              b := 28;
              while ~(!a = !b) do
              if !a > !b
              then a := !a - !b
              else b := !b - !a"""

  val EX3 =
    """x := 12;
  while !x > 1 do {
      r := !x;
      while !r > 1 do {
          r := !r - 2
          };
      if !r = 0
      then x := !x / 2
      else x := 3 * !x + 1}"""


  val EX4 =
    """x := 13;
  factor := 2;
  isprime := 1;
  limit := !x / 2 + 1;
  while !factor < !limit do {
      r := !x;
      (while !r > !factor - 1 do
          r := !r - !factor);
      (if !r = 0 then isprime := 0 else skip);
      factor := !factor + 1}"""

  val EX5 =
    """n := 10;
  a := 1;
  b := 1;
  i := 0;
  while !i < !n do {
      tmp := !a;
      a := !b;
      b := !tmp + !a;
      i := !i + 1}"""


  val arrProg1 =
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

  val arrProg2 = "arr := [1+1, 2, 3]; i1before := arr[1]; arr[1] := 10; i1after := arr[1]"


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


  val reverse_arr_prog =
    """arr := [1, 2, 3, 4, 5];
      left := 0;
      right := 4;
      while(!left < !right) do {
         tmp_left := arr[!left];
         tmp_right := arr[!right];
         arr[!right] := !tmp_left;
          arr[!left] := !tmp_right;
         left := !left + 1;
         right := !right - 1
      }
     """


  val allExamples: List[String] = List(EX1, EX2, EX3, EX4, EX5, arrProg1, arrProg2, if_true_prog, if_false_prog, reverse_arr_prog)
}
