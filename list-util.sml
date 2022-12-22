signature LIST_UTIL =
  sig
    val count : ('a -> bool) -> 'a list -> int
    val locate : ('a -> bool) -> 'a list -> int
    val group : int -> 'a list -> 'a list list
    val window : int -> 'a list -> 'a list list
    val split : ('a -> bool) -> 'a list -> 'a list list
    val mapNth : int * ('a -> 'a) -> 'a list -> 'a list
  end

structure ListUtil :> LIST_UTIL =
  struct
    fun count f l = length (List.filter f l)

    fun locate f l =
      let
        fun lp (_, []) = ~1
          | lp (i, x :: xs) = if f x then i else lp (i + 1, xs)
      in
        lp (0, l)
      end

    fun group n l =
      let
        fun lp ([], ls) = rev ls
          | lp (l, ls) = lp (List.drop (l, n), List.take (l, n) :: ls)
      in
        lp (l, [])
      end

    fun window n l =
      let
        fun lp (l, ls) =
          if length l <= n then
            rev (l :: ls)
          else
            lp (tl l, List.take (l, n) :: ls)
      in
        lp (l, [])
      end

    fun split f l =
      let
        fun sp ([], acc, ls) = rev (rev acc :: ls)
          | sp (x :: xs, acc, ls) =
            if f x then
              sp (xs, [], rev acc :: ls)
            else
              sp (xs, x :: acc, ls)
      in
        sp (l, [], [])
      end

    fun mapNth (i, f) l = List.mapi (fn (j, x) => if j = i then f x else x) l
  end
