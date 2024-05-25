signature GAP_BUFFER =
sig
  type t = {idx: int, left: string list, right: string list}
  val empty: t
  val fromString: string -> t
  val toString: t -> string
  val insert: int * string * t -> t
end

structure GapBuffer: GAP_BUFFER =
struct
  type t = {idx: int, left: string list, right: string list}

  val targetLength = 1024

  val empty = {idx = 0, left = [], right = []}

  fun fromString string =
    {idx = String.size string, left = [string], right = []}

  local
    fun toList (acc, input) =
      case input of
        hd :: tl => toList (hd :: acc, tl)
      | [] => acc
  in
    fun toString ({left, right, ...}: t) =
      let val lst = toList (right, left)
      in String.concat lst
      end
  end

  fun isLessThanTarget (s1, s2) =
    String.size s1 + String.size s2 <= targetLength

  fun isThreeLessThanTarget (s1, s2, s3) =
    String.size s1 + String.size s2 + String.size s3 <= targetLength

  fun consLeft (curIdx, newString, left, right) =
    { idx = curIdx + String.size newString
    , left = newString :: left
    , right = right
    }

  fun preferInsertLeft (curIdx, newString, left, right) =
    case left of
      hd :: tail =>
        if isLessThanTarget (hd, newString) then
          { idx = curIdx + String.size newString
          , left = (hd ^ newString) :: tail
          , right = right
          }
        else
          (case right of
             hd :: tail =>
               if isLessThanTarget (hd, newString) then
                 {idx = curIdx, left = left, right = (newString ^ hd) :: tail}
               else
                 consLeft (curIdx, newString, left, right)
           | [] => consLeft (curIdx, newString, left, right))
    | [] => consLeft (curIdx, newString, left, right)

  fun joinEndOfLeft (newString, left) =
    case left of
      hd :: tail =>
        if isLessThanTarget (newString, hd) then (hd ^ newString) :: tail
        else newString :: left
    | [] => newString :: left

  fun joinStartOfRight (newString, right) =
    case right of
      hd :: tail =>
        if isLessThanTarget (newString, hd) then (newString ^ hd) :: tail
        else newString :: right
    | [] => newString :: right

  fun ins (idx, newString, curIdx, left, right) : t =
    if curIdx = idx then
      preferInsertLeft (curIdx, newString, left, right)
    else if idx < curIdx then
      (* Need to insert on the left. *)
      case left of
        [] =>
          (* If there is no string on the left, then add the new string there. *)
          {idx = String.size newString, left = [newString], right = right}
      | hd :: tail =>
          let
            val prevIdx = curIdx - String.size hd
          in
            if
              idx < prevIdx
            then
              (* The requested index is prior to the string on the left,
               * so move leftward one string. *)
              ins (idx, newString, prevIdx, tail, joinStartOfRight (hd, right))
            else
              (* The requested index is either:
               *  - At the start of the left string
               *  - In the middle of the left string
               *  Find out which and split the middle of the string if necessary. *) if
              idx = prevIdx
            then
              (* At start of string. *)
              { idx = curIdx + String.size newString
              , right = right
              , left =
                  (* These two meant to look reversed, 
                   * with respect to newString and hd.
                   *
                   * The line
                   *   `newString ^ hd`
                   * places the contents of newString before hd,
                   * and the line
                   *   `hd :: newString`
                   * in a zipper also places newString before hd.
                   *
                   * Using `newString ^ hd` with `newString :: hd` gives
                   * different contents in the case of a zipper.
                   * *)
                  if isLessThanTarget (newString, hd) then
                    (newString ^ hd) :: tail
                  else
                    hd :: newString :: tail
              }
            else
              (* In middle of string. *)
              let
                val length = idx - prevIdx
                val sub1 = String.substring (hd, 0, length)
                val sub2 = String.substring
                  (hd, length, String.size hd - length)
              in
                if isThreeLessThanTarget (sub1, newString, sub2) then
                  { idx = curIdx + String.size newString
                  , left = (sub1 ^ newString ^ sub2) :: tail
                  , right = right
                  }
                else if isLessThanTarget (sub1, newString) then
                  { idx = prevIdx + String.size sub1 + String.size newString
                  , left = (sub1 ^ newString) :: tail
                  , right = joinStartOfRight (sub2, right)
                  }
                else if isLessThanTarget (newString, sub2) then
                  { idx = prevIdx + String.size sub1
                  , left = sub1 :: tail
                  , right = (newString ^ sub2) :: right
                  }
                else
                  { idx = prevIdx
                  , left = tail
                  , right = sub1 :: newString :: sub2 :: right
                  }
              end
          end
    else
      (* Need to insert to the right. *)
      case right of
        [] => {idx = curIdx, left = left, right = [newString]}
      | hd :: tail =>
          let
            val nextIdx = String.size hd + curIdx
          in
            if idx > nextIdx then
              ins (idx, newString, nextIdx, joinEndOfLeft (hd, left), tail)
            else if idx = nextIdx then
              (* At end of next string. *)
              if isLessThanTarget (newString, hd) then
                {idx = curIdx, left = left, right = (hd ^ newString) :: tail}
              else
                { idx = curIdx
                , left = left
                , right = hd :: (joinStartOfRight (newString, tail))
                }
            else
              let
                val length = idx - curIdx
                val sub1 = String.substring (hd, 0, length)
                val sub2 = String.substring
                  (hd, length, String.size hd - length)
              in
                if isThreeLessThanTarget (sub1, newString, sub2) then
                  { idx =
                      curIdx + String.size sub1 + String.size newString
                      + String.size sub2
                  , left = (sub1 ^ newString ^ sub2) :: left
                  , right = tail
                  }
                else if isLessThanTarget (sub1, newString) then
                  { idx = curIdx + String.size sub1 + String.size newString
                  , left = (sub1 ^ newString) :: left
                  , right = joinStartOfRight (sub2, tail)
                  }
                else if isLessThanTarget (newString, sub2) then
                  { idx = curIdx + String.size sub1
                  , left = sub1 :: left
                  , right = (newString ^ sub2) :: tail
                  }
                else
                  { idx = curIdx + String.size sub1 + String.size newString
                  , left = newString :: sub1 :: left
                  , right = joinStartOfRight (sub2, tail)
                  }
              end
          end

  fun insert (idx, newString, buffer: t) =
    ins (idx, newString, #idx buffer, #left buffer, #right buffer)
end
