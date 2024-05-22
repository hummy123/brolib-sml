structure GapBuffer =
struct
  type t = {idx: int, left: string list, right: string list}

  val targetLength = 1024

  val empty = {idx = 0, left = [], right = []}

  fun fromString string = {idx = 0, left = [], right = [string]}

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

  fun ins (idx, newString, curIdx, left, right) : t =
    if curIdx = idx then
      case left of
        [] => {idx = String.size newString, left = [newString], right = right}
      | hd :: tail =>
          if isLessThanTarget (hd, newString) then
            { idx = curIdx + String.size newString
            , left = (hd ^ newString) :: tail
            , right = right
            }
          else
            { idx = curIdx + String.size newString
            , left = newString :: left
            , right = right
            }
    else if curIdx < idx then
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
              prevIdx < idx
            then
              (* The requested index is prior to the string on the left,
               * so move leftward one string. *)
              ins (idx, newString, curIdx - String.size hd, tail, hd :: right)
            else
              (* The requested index is either:
               *  - At the start of the left string
               *  - In the middle of the left string
               *  Find out which and split the middle of the string if necessary. *) if
              idx = prevIdx
            then
              (* At start of string. *)
              if isLessThanTarget (hd, newString) then
                {idx = prevIdx, left = tail, right = (newString ^ hd) :: right}
              else
                { idx = prevIdx + String.size newString
                , left = newString :: tail
                , right = hd :: right
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
                  { idx = prevIdx
                  , left = tail
                  , right = (sub1 ^ newString ^ sub2) :: right
                  }
                else if isLessThanTarget (sub1, newString) then
                  { idx = prevIdx + String.size sub1 + String.size newString
                  , left = (sub1 ^ newString) :: tail
                  , right = sub2 :: right
                  }
                else if isLessThanTarget (newString, sub2) then
                  { idx = prevIdx + String.size sub1
                  , left = sub1 :: tail
                  , right = (newString ^ sub2) :: right
                  }
                else
                  { idx = prevIdx + String.size sub1 + String.size newString
                  , left = newString :: sub1 :: tail
                  , right = sub2 :: right
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
            if nextIdx > idx then
              ins (idx, newString, nextIdx, hd :: left, tail)
            else
              let
                val length = nextIdx - idx
                val sub1 = String.substring (hd, 0, length)
                val sub2 = String.substring
                  (hd, length, String.size hd - length)
              in
                if isThreeLessThanTarget (sub1, newString, sub2) then
                  { idx = curIdx
                  , left = left
                  , right = (sub1 ^ newString ^ sub2) :: tail
                  }
                else if isLessThanTarget (sub1, newString) then
                  { idx = curIdx + String.size sub1 + String.size newString
                  , left = (sub1 ^ newString) :: left
                  , right = sub2 :: tail
                  }
                else if isLessThanTarget (newString, sub2) then
                  { idx = curIdx + String.size sub1
                  , left = sub1 :: left
                  , right = (newString ^ sub2) :: tail
                  }
                else
                  { idx = curIdx + String.size sub1 + String.size newString
                  , left = newString :: sub1 :: left
                  , right = sub2 :: tail
                  }
              end
          end

  fun insert (idx, newString, buffer: t) =
    ins (idx, newString, #idx buffer, #left buffer, #right buffer)

end
