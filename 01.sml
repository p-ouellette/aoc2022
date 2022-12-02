val readLines = (String.fields (fn c => c = #"\n")) o TextIO.inputAll
val parseInput = (map Int.fromString) o readLines
fun readInput f = IOUtil.withInputFile (f, parseInput) TextIO.stdIn
val input = readInput "01.in"

fun part1 lis =
  let
    val (_, max) =
      foldl
        (fn (SOME n, (curr, max)) => (curr + n, Int.max (max, curr + n))
          | (NONE, (_, max)) => (0, max))
        (0, 0)
        lis
  in
    max
  end

fun part2 lis =
  let
    val (_, (a, b, c)) =
      foldl
        (fn (SOME n, (curr, (a, b, c))) =>
           let
             val curr = curr + n
             val top =
               if curr > a then
                 (curr, a, b)
               else if curr > b then
                 (a, curr, b)
               else if curr > c then
                 (a, b, curr)
               else
                 (a, b, c)
           in
             (curr, top)
           end
          | (NONE, (_, top)) => (0, top))
        (0, (0, 0, 0))
        lis
  in
    a + b + c
  end

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
