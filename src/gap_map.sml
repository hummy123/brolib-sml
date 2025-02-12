signature GAP_MAP_PAIR =
sig
  type key
  type value

  val l: key * key -> bool
  val eq: key * key -> bool
  val g: key * key -> bool

  val maxNodeSize: int
end

signature GAP_MAP =
sig
  structure Fn: GAP_MAP_PAIR

  type t

  val empty: t
  val isEmpty: t -> bool

  val add: Fn.key * Fn.value * t -> t
  val remove: Fn.key * t -> t

  val get: Fn.key * t -> Fn.value option
  val min: t -> (Fn.key * Fn.value) option
  val max: t -> (Fn.key * Fn.value) option

  val moveToStart: t -> t
  val moveToEnd: t -> t
  val moveTo: Fn.key * t -> t
end

functor MakeGapMap(Fn: GAP_MAP_PAIR): GAP_MAP =
struct
  structure Fn = Fn

  type t =
    { leftKeys: Fn.key vector list
    , letVals: Fn.value vector list
    , rightKeys: Fn.key vector list
    , rightVals: Fn.value vector list
    }

  val empty = {leftKeys = [], leftVals = [], rightKeys = [], rightVals = []}

  fun isEmpty {leftKeys = [], rightKeys = [], ...} = true
    | isEmpty _ = false

  fun isLessThanTarget (v1, v2) =
    Vector.length v1 + Vector.length v2 <= Fn.maxNodeSize
end
