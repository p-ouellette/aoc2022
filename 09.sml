use "advent-prelude.sml";

val parseInput = (map (fn s => (String.sub (s, 0), hd (ints s)))) o readLines
val moves = withInputFile ("09.in", parseInput)

fun move #"L" = Coord.left
  | move #"U" = Coord.up
  | move #"R" = Coord.right
  | move #"D" = Coord.down

fun stepT (t, h) =
  let val d as (dx, dy) = V2.sub (t, h) in
    if abs dx > 1 orelse abs dy > 1 then V2.sub (t, V2.map Int.sign d) else t
  end

fun step dir (h :: ts, visited) =
  let val r = foldl (fn (t, r) => stepT (t, hd r) :: r) [move dir h] ts in
    (rev r, V2Set.add (visited, hd r))
  end

fun simulate ((dir, cnt), acc) = Fn.repeat cnt (step dir) acc

fun countTailPos len =
  let val r = List.tabulate (len, Fn.const V2.zero) in
    V2Set.numItems o # 2 o (foldl simulate (r, V2Set.empty))
  end

val part1 = countTailPos 2
val part2 = countTailPos 10

val _ = print (Int.toString (part1 moves) ^ "\n")
val _ = print (Int.toString (part2 moves) ^ "\n")
