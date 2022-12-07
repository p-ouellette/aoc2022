val initStacks =
  [ [#"N", #"V", #"C", #"S"]
  , [#"S", #"N", #"H", #"J", #"M", #"Z"]
  , [#"D", #"N", #"J", #"G", #"T", #"C", #"M"]
  , [#"M", #"R", #"W", #"J", #"F", #"D", #"T"]
  , [#"H", #"F", #"P"]
  , [#"J", #"H", #"Z", #"T", #"C"]
  , [#"Z", #"L", #"S", #"F", #"Q", #"R", #"P", #"D"]
  , [#"G", #"P", #"F", #"D", #"H", #"L", #"S", #"C"]
  , [#"Z", #"G", #"N", #"F", #"P", #"M", #"S", #"D"]
  ]

val readLines = (String.tokens (fn c => c = #"\n")) o TextIO.inputAll
fun parseStep line =
  let
    val move :: cnt :: from :: a :: to :: b :: _ =
      String.tokens Char.isSpace line
    val int = valOf o Int.fromString
  in
    (int cnt, int a, int b)
  end
val parseInput = (map parseStep) o (fn l => List.drop (l, 9)) o readLines
fun readInput f = IOUtil.withInputFile (f, parseInput) TextIO.stdIn
val input = readInput "05.in"

fun applyStep ((cnt, from, to), stacks) =
  let
    val fromStack = List.nth (stacks, from - 1)
    val toStack = List.nth (stacks, to - 1)
  in
    List.update
      ( List.update (stacks, from - 1, List.drop (fromStack, cnt))
      , to - 1
      , List.revAppend (List.take (fromStack, cnt), toStack)
      )
  end

val part1 = implode o (map hd) o (foldl applyStep initStacks)

fun applyStep' ((cnt, from, to), stacks) =
  let
    val fromStack = List.nth (stacks, from - 1)
    val toStack = List.nth (stacks, to - 1)
  in
    List.update
      ( List.update (stacks, from - 1, List.drop (fromStack, cnt))
      , to - 1
      , List.take (fromStack, cnt) @ toStack
      )
  end

val part2 = implode o (map hd) o (foldl applyStep' initStacks)

val _ = print (part1 input ^ "\n")
val _ = print (part2 input ^ "\n")
