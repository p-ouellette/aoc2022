structure Set = IntListSet

val parseInput = explode o TextIO.inputAll
fun readInput f = IOUtil.withInputFile (f, parseInput) TextIO.stdIn
val input = readInput "06.in"

fun firstMarker (buf, len) =
  let
    fun loop (buf, i) =
      if Set.numItems (Set.fromList (map ord (List.take (buf, len)))) = len then
        i
      else
        loop (tl buf, i + 1)
  in
    loop (buf, len)
  end

fun part1 buf = firstMarker (buf, 4)
fun part2 buf = firstMarker (buf, 14)

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
