use "advent-prelude.sml";

val input = withInputFile ("02.in", readLines)

val score =
  fn "A X" => 4
   | "A Y" => 8
   | "A Z" => 3
   | "B X" => 1
   | "B Y" => 5
   | "B Z" => 9
   | "C X" => 7
   | "C Y" => 2
   | "C Z" => 6

val part1 = sum o (map score)

val score' =
  fn "A X" => 3
   | "A Y" => 4
   | "A Z" => 8
   | "B X" => 1
   | "B Y" => 5
   | "B Z" => 9
   | "C X" => 2
   | "C Y" => 6
   | "C Z" => 7

val part2 = sum o (map score')

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
