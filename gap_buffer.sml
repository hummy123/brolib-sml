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

  fun insLeft (prevIdx, idx, newString, curIdx, hd, tail, right) =
    (* The requested index is either:
         *  - At the start of the left string
         *  - In the middle of the left string
         *  Find out which and split the middle of the string if necessary. *)
    if idx = prevIdx then
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
          if isLessThanTarget (newString, hd) then (newString ^ hd) :: tail
          else hd :: newString :: tail
      }
    else
      (* In middle of string. *)
      let
        val length = idx - prevIdx
        val sub1 = String.substring (hd, 0, length)
        val sub2 = String.substring (hd, length, String.size hd - length)
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
          , left = joinEndOfLeft (sub1, tail)
          , right = (newString ^ sub2) :: right
          }
        else
          { idx = prevIdx
          , left = tail
          , right = sub1 :: newString :: sub2 :: right
          }
      end

  fun insRight (nextIdx, idx, newString, curIdx, left, hd, tail) =
    if idx = nextIdx then
      (* At end of next string. *)
      { idx = curIdx
      , left = left
      , right =
          if isLessThanTarget (newString, hd) then (hd ^ newString) :: tail
          else hd :: (joinStartOfRight (newString, tail))
      }
    else
      let
        val length = idx - curIdx
        val sub1 = String.substring (hd, 0, length)
        val sub2 = String.substring (hd, length, String.size hd - length)
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
            if idx < prevIdx then
              (* The requested index is prior to the string on the left,
               * so move leftward one string. *)
              ins (idx, newString, prevIdx, tail, joinStartOfRight (hd, right))
            else
              insLeft (prevIdx, idx, newString, curIdx, hd, tail, right)
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
            else
              insRight (nextIdx, idx, newString, curIdx, left, hd, tail)
          end

  fun insert (idx, newString, buffer: t) =
    ins (idx, newString, #idx buffer, #left buffer, #right buffer)

  fun delRightFromHere (curIdx, finish, right) =
    case right of
      hd :: tail =>
        if curIdx + String.size hd < finish then
          delRightFromHere (curIdx + String.size hd, finish, tail)
        else if curIdx + String.size hd > finish then
          let
            val newStrStart = finish - curIdx
            val newStr = String.sub
              (hd, newStrStart, String.size hd - newStrStart)
          in
            newStr :: tail
          end
        else
          (*
            Else branch implies the following is true:
              if curIdx + String.size hd = finish then 
          *)
          tail
    | [] => right

  fun del (start, finish, curIdx, left, right) : t =
    if start > curIdx then
      (* If start is greater than current index, 
       * then finish must be greater too. 
       * Move buffer rightwards until finish is reached, 
       * and delete along the way. *)
      raise Empty
    else if start < curIdx then
      (* If start is less than current index,
       * then finish could be either less than or equal/greater 
       * than the current index. 
       * We can treat equal/greater than as one case. *)
      if finish < curIdx then
        (* Move leftward and delete along the way. *)
        raise Empty
      else
        (* Delete rightward up to finish index,
         * and then delete leftward until start index.*)
        raise Empty
    else
      (* If start is equal to the current index, 
       * then only to examine right list. 
       * Just need to delete until reaching the finish index. *)
      { idx = curIdx
      , left = left
      , right = delRightFromHere (curIdx, finish, right)
      }

  fun delete (start, length, buffer: t) =
    if length > 0 then
      del (start, start + length, #idx buffer, #left buffer, #right buffer)
    else
      buffer
end
