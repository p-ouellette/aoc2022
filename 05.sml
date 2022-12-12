use "advent-prelude.sml";

val initStacks =
  map explode
    [ "NVCS"
    , "SNHJMZ"
    , "DNJGTCM"
    , "MRWJFDT"
    , "HFP"
    , "JHZTC"
    , "ZLSFQRPD"
    , "GPFDHLSC"
    , "ZGNFPMSD"
    ]

val parseInput = (map ints) o (drop 10) o readLines
val input = withInputFile ("05.in", parseInput)

fun moveCrates put ([cnt, src, dst], stacks) =
  mapNth (dst - 1, fn l => put (take cnt (List.nth (stacks, src - 1)), l))
    (mapNth (src - 1, drop cnt) stacks)

val part1 = implode o (map hd) o (foldl (moveCrates List.revAppend) initStacks)
val part2 = implode o (map hd) o (foldl (moveCrates op @) initStacks)

val _ = print (part1 input ^ "\n")
val _ = print (part2 input ^ "\n")
