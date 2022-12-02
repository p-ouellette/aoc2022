val readLines = (String.fields (fn c => c = #"\n")) o TextIO.inputAll
fun parseInput f = readLines f
fun readInput f = IOUtil.withInputFile (f, parseInput) TextIO.stdIn
val input = readInput "01.in"

fun part1 lines =
  let
    val (_, max) =
      foldl
        (fn (line, (curr, max)) =>
           (case Int.fromString line of
              SOME n => (curr + n, Int.max (max, curr + n))
            | NONE => (0, max)))
        (0, 0)
        lines
  in
    max
  end

fun part2 lines =
  let
    val (_, (a, b, c)) =
      foldl
        (fn (line, (curr, (a, b, c))) =>
           (case Int.fromString line of
              SOME n =>
                let
                  val curr = curr + n
                  val max =
                    if curr > a then
                      (curr, a, b)
                    else if curr > b then
                      (a, curr, b)
                    else if curr > c then
                      (a, b, curr)
                    else
                      (a, b, c)
                in
                  (curr, max)
                end
            | NONE => (0, (a, b, c))))
        (0, (0, 0, 0))
        lines
  in
    a + b + c
  end

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
