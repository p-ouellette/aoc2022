signature VECTOR2 =
  sig
    type elem
    type vec = elem * elem

    val zero : vec

    val add : vec * vec -> vec
    val sub : vec * vec -> vec

    val map : (elem -> elem) -> vec -> vec

    val collate : (elem * elem -> order) -> vec * vec -> order
  end

structure IntVector2 :> VECTOR2 where type elem = int =
  struct
    type elem = int
    type vec = int * int

    val zero = (0, 0)

    fun add ((x1, y1), (x2, y2)) = (x1 + x2, y1 + y2)
    fun sub ((x1, y1), (x2, y2)) = (x1 - x2, y1 - y2)

    fun map f (x, y) = (f x, f y)

    fun collate f ((x1, y1), (x2, y2)) =
      case f (x1, x2) of
        EQUAL => f (y1, y2)
      | order => order
  end
