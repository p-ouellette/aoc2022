use "advent-prelude.sml";

val parseInput = V.fromList o (map (V.fromList o digits)) o readLines
val hmap = withInputFile ("08.in", parseInput)

fun coords hmap =
  List.concatMap
    (fn r => List.tabulate (V.length (V.sub (hmap, 0)), fn c => (r, c)))
    (List.tabulate (V.length hmap, Fn.id))

fun vsub i v = V.sub (v, i)
fun vsub2 (v, (x, y)) = V.sub (V.sub (v, x), y)

fun lines hmap (x, y) =
  [ VS.slice (vsub x hmap, 0, SOME y)
  , VS.slice (V.map (vsub y) hmap, 0, SOME x)
  , VS.slice (vsub x hmap, y + 1, NONE)
  , VS.slice (V.map (vsub y) hmap, x + 1, NONE)
  ]

fun visible hmap pos =
  let val height = vsub2 (hmap, pos) in
    List.exists (VS.all (fn h => h < height)) (lines hmap pos)
  end

fun part1 hmap = count (visible hmap) (coords hmap)

fun score hmap pos =
  let
    val height = vsub2 (hmap, pos)
    fun look1 (h, d) = if h < height then d + 1 else 1
    val lookl = VS.foldr look1 0
    val lookr = VS.foldl look1 0
    val [lt, top, rt, bot] = lines hmap pos
  in
    foldl op * 1 [lookr lt, lookr top, lookl rt, lookl bot]
  end

fun part2 hmap = max (map (score hmap) (coords hmap))

val _ = print (Int.toString (part1 hmap) ^ "\n")
val _ = print (Int.toString (part2 hmap) ^ "\n")
