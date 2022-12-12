use "advent-prelude.sml";

val parseInput = (map ints) o readLines
val input = withInputFile ("04.in", parseInput)

val part1 =
  count (fn [a, b, c, d] => a >= c andalso b <= d orelse c >= a andalso d <= b)

val part2 = count (fn [a, b, c, d] => not (c > b orelse a > d))

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
