use "advent-prelude.sml";

datatype pval = I of int | L of pval list

fun listParser getc =
  ListFormat.scan {init = "[", sep = ",", final = "]", scan = pvalParser} getc
and pvalParser getc =
  P.or (P.wrap (listParser, L), P.wrap (Int.scan StringCvt.DEC, I)) getc

val parsePacket = valOf o (StringCvt.scanString listParser)
val parseInput = (map (map parsePacket)) o (LU.split (Fn.equal "")) o readLines
val input = withInputFile ("13.in", parseInput)

fun cmpList (l, r) = List.collate cmpPval (l, r)

and cmpPval (I l, I r) = Int.compare (l, r)
  | cmpPval (L l, L r) = cmpList (l, r)
  | cmpPval (I l, r) = cmpPval (L [I l], r)
  | cmpPval (l, I r) = cmpPval (l, L [I r])

fun packetLess (a, b) = cmpList (a, b) = LESS

val part1 =
  List.foldli
    (fn (i, [a, b], acc) => if packetLess (a, b) then acc + i + 1 else acc) 0

fun part2 input =
  let
    val d1 = [L [I 2]]
    val d2 = [L [I 6]]
    val l = Sort.sort (not o packetLess) (d1 :: d2 :: List.concat input)
  in
    (locate (Fn.equal d1) l + 1) * (locate (Fn.equal d2) l + 1)
  end

val _ = print (Int.toString (part1 input) ^ "\n")
val _ = print (Int.toString (part2 input) ^ "\n")
