val readLines = (String.tokens (fn c => c = #"\n")) o TextIO.inputAll
fun parsePair line =
  let
    val parseRange =
      (map (valOf o Int.fromString)) o (String.fields (fn c => c = #"-"))
    val pair = String.fields (fn c => c = #",") line
    val (a :: b :: _) :: (x :: y :: _) :: _ = map parseRange pair
  in
    ((a, b), (x, y))
  end
val parseInput = (map parsePair) o readLines
fun readInput f = IOUtil.withInputFile (f, parseInput) TextIO.stdIn
val input = readInput "04.in"

fun cont ((a, b), c) = c >= a andalso c <= b
fun contRange (a, (x, y)) = cont (a, x) andalso cont (a, y)

val part1 =
  foldl
    (fn ((a, b), n) =>
       if contRange (a, b) orelse contRange (b, a) then
         n + 1
       else
         n)
    0

fun overlap ((a, b), (x, y)) = cont ((a, b), x) orelse cont ((x, y), a)

val part2 = foldl (fn ((a, b), n) => if overlap (a, b) then n + 1 else n) 0

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
