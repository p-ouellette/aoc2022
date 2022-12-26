use "list-util.sml";
use "string-util.sml";
use "int-vector2.sml";
use "coord.sml";
use "grid.sml";

structure Set = IntRedBlackSet
structure Sort = ListMergeSort
structure TIO = TextIO
structure V = Vector
structure VS = VectorSlice

structure V2 = IntVector2
structure V2Set =
  RedBlackSetFn (type ord_key = V2.t val compare = V2.collate Int.compare)

open ListUtil
open StringUtil

val sum = foldl op+ 0
fun min (x :: xs) = foldl Int.min x xs
fun max (x :: xs) = foldl Int.max x xs
val reduce = List.reduce
val filter = List.filter
fun take n l = List.take (l, n)
fun drop n l = List.drop (l, n)

local
  fun intToks f = (map (valOf o Int.fromString)) o (String.tokens f)
in
  val ints = intToks (not o Char.isDigit)
  val signedInts = intToks (fn c => not (Char.isDigit c orelse c = #"-"))
end

val digits = (map (fn c => ord c - ord #"0")) o explode

fun withInputFile (file, f) = IOUtil.withInputFile (file, f) TIO.stdIn

fun readLines strm =
  let val l = String.fields (Fn.equal #"\n") (TIO.inputAll strm)
  in List.take (l, length l - 1)
  end
