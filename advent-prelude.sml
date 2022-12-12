use "string-util.sml";

structure Set = IntListSet
structure Sort = ListMergeSort
structure TIO = TextIO

(* list utils *)

val sum = foldl op + 0

fun reduce f id [] = id
  | reduce f _ (x :: xs) = foldl f x xs

fun count f l = length (List.filter f l)

fun locate f l =
  let
    fun lp (_, []) = ~1
      | lp (i, x :: xs) = if f x then i else lp (i + 1, xs)
  in
    lp (0, l)
  end

fun take n l = List.take (l, n)
fun drop n l = List.drop (l, n)

fun group n l =
  let
    fun lp ([], ls) = rev ls
      | lp (l, ls) = lp (drop n l, take n l :: ls)
  in
    lp (l, [])
  end

fun window n l =
  let
    fun lp (l, ls) =
      if length l <= n then
        rev (l :: ls)
      else
        lp (tl l, take n l :: ls)
  in
    lp (l, [])
  end

fun mapNth (i, f) = List.mapi (fn (i', x) => if i' = i then f x else x)

(* parsing and string utils *)

val ints = (map (valOf o Int.fromString)) o (String.tokens (not o Char.isDigit))

val split = StringUtil.split

(* file reading *)

fun withInputFile (file, f) = IOUtil.withInputFile (file, f) TIO.stdIn

fun readLines strm =
  let val l = String.fields (fn c => c = #"\n") (TIO.inputAll strm) in
    take (length l - 1) l
  end
