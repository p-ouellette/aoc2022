signature GRID =
sig
  type 'a t

  val fromList: 'a list list -> 'a t

  val sub: 'a t * Coord.t -> 'a

  val nRows: 'a t -> int
  val nCols: 'a t -> int

  val row: 'a t * int -> 'a vector
  val col: 'a t * int -> 'a vector

  val adj: 'a t * Coord.t -> Coord.t list

  val foldi: (Coord.t * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b

  val locate: ('a -> bool) -> 'a t -> Coord.t
end

structure Grid :> GRID =
struct
  structure A2 = Array2

  type 'a t = 'a A2.array

  val fromList = A2.fromList

  fun sub (g, (x, y)) = A2.sub (g, x, y)

  val nRows = A2.nRows
  val nCols = A2.nCols

  val row = A2.row
  val col = A2.column

  fun adj (g, p) =
    List.filter
      (fn (x, y) =>
         x >= 0 andalso x < nRows g andalso y >= 0 andalso y < nCols g)
      [Coord.left p, Coord.up p, Coord.right p, Coord.down p]

  fun foldi f init g =
    A2.foldi A2.RowMajor (fn (x, y, a, b) => f ((x, y), a, b)) init
      {base = g, row = 0, col = 0, nrows = NONE, ncols = NONE}

  fun locate f g =
    let
      fun loc x =
        if x >= A2.nRows g then
          raise Fail "no matching element"
        else
          case Vector.findi (f o #2) (A2.row (g, x)) of
            NONE => loc (x + 1)
          | SOME (y, _) => (x, y)
    in
      loc 0
    end
end
