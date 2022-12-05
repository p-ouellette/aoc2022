structure Set = IntListSet

val readLines = (String.tokens (fn c => c = #"\n")) o TextIO.inputAll
val parseInput = (map ((map ord) o explode)) o readLines
fun readInput f = IOUtil.withInputFile (f, parseInput) TextIO.stdIn
val input = readInput "03.in"

fun sharedItem items =
  let
    val half = length items div 2
    val fst = Set.fromList (List.take (items, half))
    val snd = Set.fromList (List.drop (items, half))
  in
    Set.minItem (Set.intersection (fst, snd))
  end

fun priority item =
  item + (if item >= ord #"a" then 1 - ord #"a" else 27 - ord #"A")

fun part1 sacks = foldl op + 0 (map (priority o sharedItem) sacks)

fun badge (a, b, c) =
  Set.minItem
    (Set.intersection
       (Set.intersection (Set.fromList a, Set.fromList b), Set.fromList c))

fun part2 sacks =
  let
    fun loop (a :: b :: c :: tl, n) = loop (tl, n + priority (badge (a, b, c)))
      | loop (_, n) = n
  in
    loop (sacks, 0)
  end

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
