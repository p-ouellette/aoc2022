use "advent-prelude.sml";

val parseInput = Grid.fromList o (map digits) o readLines
val hmap = withInputFile ("08.in", parseInput)

fun lines g (x, y) =
  [ VS.slice (Grid.row (g, x), 0, SOME y)
  , VS.slice (Grid.col (g, y), 0, SOME x)
  , VS.slice (Grid.row (g, x), y + 1, NONE)
  , VS.slice (Grid.col (g, y), x + 1, NONE)
  ]

fun visible (g, p, h) = List.exists (VS.all (fn h' => h' < h)) (lines g p)

fun part1 g =
  Grid.foldi (fn (p, h, n) => if visible (g, p, h) then n + 1 else n) 0 g

fun score (g, p, h) =
  let
    fun look1 (h', d) = if h' < h then d + 1 else 1
    val lookl = VS.foldr look1 0
    val lookr = VS.foldl look1 0
    val [lt, top, rt, bot] = lines g p
  in
    foldl op * 1 [lookr lt, lookr top, lookl rt, lookl bot]
  end

fun part2 g = Grid.foldi (fn (p, h, s) => Int.max (s, score (g, p, h))) 0 g

val _ = print (Int.toString (part1 hmap) ^ "\n")
val _ = print (Int.toString (part2 hmap) ^ "\n")
