use "advent-prelude.sml";

val parseInput = (map ord) o explode o TIO.inputAll
val input = withInputFile ("06.in", parseInput)

fun firstMarker (buf, len) =
  len
  +
  locate (fn l => Set.numItems (Set.fromList (take len l)) = len)
    (window len buf)

fun part1 buf = firstMarker (buf, 4)
fun part2 buf = firstMarker (buf, 14)

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
