signature VECTOR2 =
sig
  type elem
  type t = elem * elem

  val zero: t

  val add: t * t -> t
  val sub: t * t -> t

  val map: (elem -> elem) -> t -> t

  val collate: (elem * elem -> order) -> t * t -> order
end

structure IntVector2 :> VECTOR2 where type elem = int =
struct
  type elem = int
  type t = int * int

  val zero = (0, 0)

  fun add ((x1, y1), (x2, y2)) = (x1 + x2, y1 + y2)
  fun sub ((x1, y1), (x2, y2)) = (x1 - x2, y1 - y2)

  fun map f (x, y) = (f x, f y)

  fun collate f ((x1, y1), (x2, y2)) =
    case f (x1, x2) of
      EQUAL => f (y1, y2)
    | order => order
end
