use "list-util.sml";
use "string-util.sml";

structure Set = IntListSet
structure Sort = ListMergeSort
structure TIO = TextIO

open ListUtil
open StringUtil

fun id x = x

val sum = foldl op + 0
val reduce = List.reduce
val filter = List.filter
fun nth n l = List.nth (l, n)
fun take n l = List.take (l, n)
fun drop n l = List.drop (l, n)

val ints = (map (valOf o Int.fromString)) o (String.tokens (not o Char.isDigit))

val digits = (map (fn c => ord c - ord #"0")) o explode

fun withInputFile (file, f) = IOUtil.withInputFile (file, f) TIO.stdIn

fun readLines strm =
  let val l = String.fields (fn c => c = #"\n") (TIO.inputAll strm) in
    List.take (l, length l - 1)
  end
