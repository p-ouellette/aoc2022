signature STRING_UTIL =
sig
  val index: string -> string -> int
  val split: string -> string -> string list
end

structure StringUtil :> STRING_UTIL =
struct
  fun index substr s =
    let
      val n = String.size substr
      val n' = String.size s

      fun lp (i, j) =
        if i + n > n' then
          ~1
        else if j = n then
          i
        else if String.sub (s, i + j) = String.sub (substr, j) then
          lp (i, j + 1)
        else
          lp (i + 1, 0)
    in
      lp (0, 0)
    end

  fun split delim s =
    let
      fun lp (s, sl) =
        case index delim s of
          ~1 => rev (s :: sl)
        | 0 => lp (String.extract (s, String.size delim, NONE), "" :: sl)
        | i =>
            let val s' = String.extract (s, i + String.size delim, NONE)
            in lp (s', String.substring (s, 0, i) :: sl)
            end
    in
      lp (s, [])
    end
end
