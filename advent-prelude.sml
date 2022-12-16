use "list-util.sml";
use "string-util.sml";

structure Set = IntListSet
structure Sort = ListMergeSort
structure TIO = TextIO
structure V = Vector
structure VS = VectorSlice

open ListUtil
open StringUtil

val sum = foldl op + 0
fun min (x :: xs) = foldl Int.min x xs
fun max (x :: xs) = foldl Int.max x xs
val reduce = List.reduce
val filter = List.filter
fun take n l = List.take (l, n)
fun drop n l = List.drop (l, n)

val ints = (map (valOf o Int.fromString)) o (String.tokens (not o Char.isDigit))

val digits = (map (fn c => ord c - ord #"0")) o explode

fun withInputFile (file, f) = IOUtil.withInputFile (file, f) TIO.stdIn

fun readLines strm =
  let val l = String.fields (Fn.equal #"\n") (TIO.inputAll strm) in
    List.take (l, length l - 1)
  end
