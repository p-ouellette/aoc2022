signature COORD =
sig
  type t = int * int

  val left: t -> t
  val right: t -> t
  val up: t -> t
  val down: t -> t
end

structure Coord :> COORD =
struct
  type t = int * int

  fun left (x, y) = (x - 1, y)
  fun right (x, y) = (x + 1, y)
  fun up (x, y) = (x, y + 1)
  fun down (x, y) = (x, y - 1)
end
