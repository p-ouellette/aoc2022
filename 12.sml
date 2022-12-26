use "advent-prelude.sml";

val parseInput = Grid.fromList o (map explode) o readLines
val grid = withInputFile ("12.in", parseInput)

fun shortestPath (g, start) =
  let
    fun bfs ([], _) = NONE
      | bfs ((p, len) :: ps, visited) =
          let
            val c = Grid.sub (g, p)
          in
            if c = #"E" then
              SOME len
            else
              let
                val height = fn #"S" => 0 | #"E" => 25 | c => ord c - ord #"a"
                fun canMove p' = height c >= height (Grid.sub (g, p')) - 1
                fun isVisited p = V2Set.member (visited, p)
                val adj = List.filter canMove (Grid.adj (g, p))
                val adj = List.filter (not o isVisited) adj
                val ps' = ps @ map (fn p => (p, len + 1)) adj
              in
                bfs (ps', V2Set.addList (visited, adj))
              end
          end
  in
    bfs ([(start, 0)], V2Set.empty)
  end

fun part1 g = valOf (shortestPath (g, Grid.locate (Fn.equal #"S") g))

fun part2 g =
  let val sl = Grid.foldi (fn (p, c, l) => if c = #"a" then p :: l else l) [] g
  in min (List.mapPartial (fn p => shortestPath (g, p)) sl)
  end

val _ = print (Int.toString (part1 grid) ^ "\n")
val _ = print (Int.toString (part2 grid) ^ "\n")
