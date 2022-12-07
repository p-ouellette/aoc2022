structure P = ParserComb
structure Sort = ListMergeSort

fun numP getc = P.wrap (P.token Char.isDigit, valOf o Int.fromString) getc
fun numListP getc = P.oneOrMore (P.seqWith # 1 (numP, P.char #"\n")) getc
fun parser getc = P.zeroOrMore (P.skipBefore (fn c => c = #"\n") numListP) getc

val parseInput = TextIO.scanStream parser
fun readInput f = IOUtil.withInputFile (f, parseInput) TextIO.stdIn
val input = valOf (readInput "01.in")

val sum = foldl op + 0
fun take n l = List.take (l, n)

val part1 = (foldl Int.max 0) o (map sum)

val part2 = sum o (take 3) o (Sort.sort op <) o (map sum)

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
