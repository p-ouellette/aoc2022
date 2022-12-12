use "advent-prelude.sml";

val parseInput = (map ((map ord) o explode)) o readLines
val input = withInputFile ("03.in", parseInput)

fun comps items = group (length items div 2) items

val commonItem =
  Set.minItem o (reduce Set.intersection Set.empty) o (map Set.fromList)

fun priority item =
  item + (if item >= ord #"a" then 1 - ord #"a" else 27 - ord #"A")

val part1 = sum o (map (priority o commonItem o comps))
val part2 = sum o (map (priority o commonItem)) o (group 3)

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
