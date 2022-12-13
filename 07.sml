use "advent-prelude.sml";

datatype file = DIR of file list | REG of int

fun makeTree lists =
  let
    fun mk ([] :: ls, ts) = (rev ts, ls)
      | mk ((~1 :: fs) :: ls, ts) =
        let val (ts', ls') = mk (ls, []) in mk (fs :: ls', DIR ts' :: ts) end
      | mk ((f :: fs) :: ls, ts) = mk (fs :: ls, REG f :: ts)
    val (ts, _) = mk (lists, [])
  in
    DIR ts
  end

val parseInput =
  makeTree
  o (map (map ((fn [] => ~1 | [n] => n) o ints)))
  o (filter (not o null))
  o (ListUtil.split (String.isPrefix "$"))
  o readLines

val root = withInputFile ("07.in", parseInput)

fun walkTree f init tree =
  let
    fun walk (DIR fs, acc) = foldl walk (f (DIR fs, acc)) fs
      | walk (reg, acc) = f (reg, acc)
  in
    walk (tree, init)
  end

val size = walkTree (fn (REG n, acc) => acc + n | (_, acc) => acc) 0
val dirSizes = walkTree (fn (REG _, acc) => acc | (d, acc) => size d :: acc) []

val part1 = sum o (filter (fn n => n <= 100000)) o dirSizes

fun part2 root =
  let val minsz = 30000000 - (70000000 - size root) in
    min (filter (fn n => n >= minsz) (dirSizes root))
  end

val _ = print (Int.toString (part1 root) ^ "\n")
val _ = print (Int.toString (part2 root) ^ "\n")
