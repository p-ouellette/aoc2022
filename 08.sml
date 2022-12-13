use "advent-prelude.sml";

val parseInput = (map digits) o readLines
val hmap = withInputFile ("08.in", parseInput)

fun coords hmap =
  List.concatMap (fn i => List.tabulate (length (hd hmap), fn j => (i, j)))
    (List.tabulate (length hmap, id))

fun lines hmap (x, y) =
  [ rev (take y (nth x hmap))
  , rev (take x (map (nth y) hmap))
  , drop (y + 1) (nth x hmap)
  , drop (x + 1) (map (nth y) hmap)
  ]

fun visible hmap (x, y) =
  let val height = nth y (nth x hmap) in
    List.exists (List.all (fn h => h < height)) (lines hmap (x, y))
  end

fun part1 hmap = count (visible hmap) (coords hmap)

fun score hmap (x, y) =
  let
    val height = nth y (nth x hmap)
    fun look [] = 0
      | look (h :: hs) =
        if h < height then
          1 + look hs
        else if h = height then
          1
        else
          0
  in
    foldl op * 1 (map look (lines hmap (x, y)))
  end

fun part2 hmap = foldl Int.max 0 (map (score hmap) (coords hmap))

val _ = print (Int.toString (part1 hmap) ^ "\n")
val _ = print (Int.toString (part2 hmap) ^ "\n")
