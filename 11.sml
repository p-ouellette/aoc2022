use "advent-prelude.sml";

fun parseFn ["*", "old"] = (fn old => old * old)
  | parseFn ["*", x] = Fn.curry op* (valOf (Int.fromString x))
  | parseFn ["+", x] = Fn.curry op+ (valOf (Int.fromString x))

fun parseMonkey [m, items, op', test, ift, iff] =
  ( ints items
  , { n = hd (ints m)
    , op' = parseFn (drop 4 (String.tokens (Fn.equal #" ") op'))
    , test = hd (ints test)
    , ift = hd (ints ift)
    , iff = hd (ints iff)
    }
  )

val parseInput =
  ListPair.unzip o (map parseMonkey) o (LU.split (Fn.equal "")) o readLines

val input = withInputFile ("11.in", parseInput)

fun solve part (items, monkeys) =
  let
    val (rounds, d) = if part = 1 then (20, 3) else (10000, 1)
    val lcm = foldl op* 1 (map #test monkeys)

    fun lp (r, [], is, cs) = lp (r + 1, monkeys, is, cs)
      | lp (r, {n, op', test, ift, iff} :: ms, is, cs) =
          if r > rounds then
            foldl op* 1 (take 2 (Sort.sort op< cs))
          else
            let
              val mis = map (fn i => op' i div d mod lcm) (List.nth (is, n))
              val (t, f) = List.partition (fn i => i mod test = 0) mis
              val is' = List.update (is, n, [])
              val is' = mapNth (ift, fn l => l @ t) is'
              val is' = mapNth (iff, fn l => l @ f) is'
              val cs' = mapNth (n, fn c => c + length mis) cs
            in
              lp (r, ms, is', cs')
            end
  in
    lp (1, monkeys, items, List.tabulate (length items, Fn.const 0))
  end

val _ = print (Int.toString (solve 1 input) ^ "\n")
val _ = print (Int.toString (solve 2 input) ^ "\n")
