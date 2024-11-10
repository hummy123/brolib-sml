signature GAP_BUFFER =
sig
  type t = {idx: int, left: string list, right: string list}
  val empty: t
  val fromString: string -> t
  val toString: t -> string
  val insert: int * string * t -> t
  val delete: int * int * t -> t
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

  fun deleteRightFromHere (curIdx, finish, right) =
    case right of
      hd :: tail =>
        let
          val nextIdx = curIdx + String.size hd
        in
          if nextIdx < finish then
            deleteRightFromHere (nextIdx, finish, tail)
          else if nextIdx > finish then
            let
              val newStrStart = finish - curIdx
              val newStr = String.substring
                (hd, newStrStart, String.size hd - newStrStart)
            in
              newStr :: tail
            end
          else
            (* nextIdx = finish
             * Delete current head but no further. *)
            tail
        end
    | [] => right

  fun moveRightAndDelete (start, finish, curIdx, left, right) =
    case right of
      hd :: tail =>
        let
          val nextIdx = curIdx + String.size hd
        in
          if nextIdx < start then
            (* Keep moving right: haven't reached start yet. *)
            moveRightAndDelete
              (start, finish, nextIdx, joinEndOfLeft (hd, left), tail)
          else if nextIdx > start then
            if nextIdx < finish then
              (* Delete the start range contained in this string,
               * and then continue deleting right. *)
              let
                val length = start - curIdx
                val newString = String.substring (hd, 0, length)
              in
                { idx = curIdx + String.size newString
                , left = joinEndOfLeft (newString, left)
                , right = deleteRightFromHere (nextIdx, finish, tail)
                }
              end
            else if nextIdx > finish then
              (* Have to delete from middle of string. *)
              let
                val sub1Length = start - curIdx
                val sub1 = String.substring (hd, 0, sub1Length)
                val sub2Start = finish - curIdx
                val sub2 = String.substring
                  (hd, sub2Start, String.size hd - sub2Start)
              in
                { idx = curIdx + sub1Length
                , left = joinEndOfLeft (sub1, left)
                , right = joinStartOfRight (sub2, tail)
                }
              end
            else
              (* nextIdx = finish 
               * Have to delete from end of this string. *)
              let
                val strLength = start - curIdx
                val str = String.substring (hd, 0, strLength)
              in
                { idx = curIdx + strLength
                , left = joinEndOfLeft (str, left)
                , right = tail
                }
              end
          else
            (* nextIdx = start
             * The start range is contained fully at the next node,
             * without having to remove part of a string at this node.*)
            let
              val newRight = deleteRightFromHere (nextIdx, finish, tail)
            in
              { idx = curIdx
              , left = left
              , right = joinStartOfRight (hd, newRight)
              }
            end
        end
    | [] => {idx = curIdx, left = left, right = right}

  fun deleteLeftFromHere (start, curIdx, left, right) =
    case left of
      hd :: tail =>
        let
          val prevIdx = curIdx - String.size hd
        in
          if start < prevIdx then
            deleteLeftFromHere (start, prevIdx, tail, right)
          else if start > prevIdx then
            (* Need to delete from some part of this string. *)
            let
              val length = start - prevIdx
              val newStr = String.substring (hd, 0, length)
            in
              { idx = prevIdx
              , left = tail
              , right = joinStartOfRight (newStr, right)
              }
            end
          else
            (* if start = prevIdx 
             * Need to remove the current node without deleting any further. *)
            {idx = prevIdx, left = tail, right = right}
        end
    | [] => {idx = curIdx, left = left, right = right}

  fun deleteFromLeftAndRight (start, finish, curIdx, left, right) =
    let val right = deleteRightFromHere (curIdx, finish, right)
    in deleteLeftFromHere (start, curIdx, left, right)
    end

  fun moveLeftAndDelete (start, finish, curIdx, left, right) =
    case left of
      hd :: tail =>
        let
          val prevIdx = curIdx - String.size hd
        in
          if prevIdx > finish then
            moveLeftAndDelete
              (start, finish, prevIdx, tail, joinStartOfRight (hd, right))
          else if prevIdx < finish then
            if prevIdx > start then
              (* Delete from start point of this string, 
               * and then call function to continue deleting leftward. *)
              let
                val hdStart = finish - prevIdx
                val newHd = String.substring
                  (hd, hdStart, String.size hd - hdStart)
                val right = joinStartOfRight (newHd, right)
              in
                deleteLeftFromHere (start, prevIdx, tail, right)
              end
            else if prevIdx < start then
              (* We want to delete in the middle of this current string. *)
              let
                val sub1Length = start - prevIdx
                val sub1 = String.substring (hd, 0, sub1Length)
                val sub2Start = finish - prevIdx
                val sub2 = String.substring
                  (hd, sub2Start, String.size hd - sub2Start)
              in
                { idx = prevIdx + sub1Length
                , left = joinEndOfLeft (sub1, tail)
                , right = joinStartOfRight (sub2, right)
                }
              end
            else
              (* prevIdx = start 
               * We want to delete from the start of this string and stop. *)
              let
                val strStart = finish - prevIdx
                val str = String.substring
                  (hd, strStart, String.size hd - strStart)
              in
                { idx = prevIdx
                , left = tail
                , right = joinStartOfRight (str, right)
                }
              end
          else
            (* prevIdx = finish *)
            deleteLeftFromHere
              (start, prevIdx, tail, joinStartOfRight (hd, right))
        end
    | [] => {idx = curIdx, left = left, right = right}

  fun del (start, finish, curIdx, left, right) : t =
    if start > curIdx then
      (* If start is greater than current index, 
       * then finish must be greater too. 
       * Move buffer rightwards until finish is reached, 
       * and delete along the way. *)
      moveRightAndDelete (start, finish, curIdx, left, right)
    else if start < curIdx then
      (* If start is less than current index,
       * then finish could be either less than or equal/greater 
       * than the current index. 
       * We can treat equal/greater than as one case. *)
      if finish <= curIdx then
        (* Move leftward and delete along the way. *)
        moveLeftAndDelete (start, finish, curIdx, left, right)
      else
        (* Delete rightward up to finish index,
         * and then delete leftward until start index.*)
        deleteFromLeftAndRight (start, finish, curIdx, left, right)
    else
      (* If start is equal to the current index, 
       * then only examine the right list. 
       * Just need to delete until reaching the finish index. *)
      { idx = curIdx
      , left = left
      , right = deleteRightFromHere (curIdx, finish, right)
      }

  fun delete (start, length, buffer: t) =
    if length > 0 then
      del (start, start + length, #idx buffer, #left buffer, #right buffer)
    else
      buffer
end
