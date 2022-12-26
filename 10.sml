use "advent-prelude.sml";

val parseInput = (map ((fn [] => 0 | [v] => v) o signedInts)) o readLines
val ops = withInputFile ("10.in", parseInput)

fun execute (v, (cycle, x, acc)) =
  let
    fun tick (0, c, acc) = (c, x + v, acc)
      | tick (t, c, acc) =
          tick (t - 1, c + 1, if (c + 20) mod 40 = 0 then acc + c * x else acc)
  in
    tick (if v = 0 then 1 else 2, cycle, acc)
  end

val part1 = #3 o (foldl execute (1, 1, 0))

fun execute' (v, (cycle, x)) =
  let
    fun tick (0, c) = (c, x + v)
      | tick (t, c) =
          ( print (if abs (x - (c - 1) mod 40) <= 1 then "#" else ".")
          ; print (if c mod 40 = 0 then "\n" else "")
          ; tick (t - 1, c + 1)
          )
  in
    tick (if v = 0 then 1 else 2, cycle)
  end

val part2 = foldl execute' (1, 1)

val _ = print (Int.toString (part1 ops) ^ "\n")
val _ = part2 ops
