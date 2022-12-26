use "advent-prelude.sml";

val parseInput = (map ints) o (split "\n\n") o TIO.inputAll
val input = withInputFile ("01.in", parseInput)

val part1 = max o (map sum)
val part2 = sum o (take 3) o (Sort.sort op<) o (map sum)

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
