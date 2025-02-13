signature GAP_VECTOR_INPUT =
sig
  type elem

  val maxNodeSide: int
end

signature GAP_VECTOR =
sig
  structure Fn: GAP_VECTOR_INPUT

  type t = {idx: int, left: Fn.elem vector list, right: Fn.elem vector list}

  val empty: t
  val fromVector: Fn.elem vector -> t
  val toVector: t -> Fn.elem vector

  val insert: int * Fn.elem * t -> t
  val insertMany: int * Fn.elem vector * t -> t

  val deleteMany: int * int * t -> t
end

functor MakeGapVector(Fn: GAP_VECTOR_INPUT): GAP_VECTOR =
struct
  structure Fn = Fn

  type t = {idx: int, left: Fn.elem vector list, right: Fn.elem vector list}

  val empty = {idx = 0, left = [], right = []}

  fun fromVector vec = {idx = Vector.length vec, left = [vec], right = []}

  local
    fun toList (acc, input) =
      case input of
        hd :: tl => toList (hd :: acc, tl)
      | [] => acc
  in
    fun toVector ({left, right, ...}: t) =
      let val lst = toList (right, left)
      in Vector.concat lst
      end
  end

  fun isLessThanTarget (v1, v2) =
    Vector.length v1 + Vector.length v2 <= Fn.maxNodeSide

  fun isThreeLessThanTarget (v1, v2, v3) =
    Vector.length v1 + Vector.length v2 + Vector.length v3 <= Fn.maxNodeSide

  fun consLeft (curIdx, newVector, left, right) =
    { idx = curIdx + Vector.length newVector
    , left = newVector :: left
    , right = right
    }

  fun joinEndOfLeft (newVector, left) =
    case left of
      hd :: tail =>
        if isLessThanTarget (newVector, hd) then
          Vector.concat [hd, newVector] :: tail
        else
          newVector :: left
    | [] => newVector :: left

  fun joinStartOfRight (newVector, right) =
    case right of
      hd :: tail =>
        if isLessThanTarget (newVector, hd) then
          Vector.concat [newVector, hd] :: tail
        else
          newVector :: right
    | [] => newVector :: right

  fun preferInsertLeft (curIdx, newVector, left, right) =
    case left of
      hd :: tail =>
        if isLessThanTarget (hd, newVector) then
          { idx = curIdx + Vector.length newVector
          , left = Vector.concat [hd, newVector] :: tail
          , right = right
          }
        else
          (case right of
             hd :: tail =>
               if isLessThanTarget (hd, newVector) then
                 { idx = curIdx
                 , left = left
                 , right = Vector.concat [newVector, hd] :: tail
                 }
               else
                 consLeft (curIdx, newVector, left, right)
           | [] => consLeft (curIdx, newVector, left, right))
    | [] => consLeft (curIdx, newVector, left, right)

  fun isSliceLessThanTarget (slice, vec) =
    VectorSlice.length slice + Vector.length vec <= Fn.maxNodeSide

  fun isThreeSliceLessThanTarget (slice1, slice2, vec) =
    VectorSlice.length slice1 + VectorSlice.length slice2 + Vector.length vec
    <= Fn.maxNodeSide

  fun insLeft (prevIdx, idx, newVector, curIdx, hd, tail, right) =
    (* The requested index is either:
         *  - At the start of the left vector
         *  - In the middle of the left vector
         *  Find out which and split the middle of the vector if necessary. *)
    if idx = prevIdx then
      (* At start of vector. *)
      { idx = curIdx + Vector.length newVector
      , right = right
      , left =
          (* These two meant to look reversed, 
           * with respect to newVector and hd.
           *
           * The line
           *   `newVector ^ hd`
           * places the contents of newVector before hd,
           * and the line
           *   `hd :: newVector`
           * in a zipper also places newVector before hd.
           *
           * Using `newVector ^ hd` with `newVector :: hd` gives
           * different contents in the case of a zipper.
           * *)
          if isLessThanTarget (newVector, hd) then
            Vector.concat [newVector, hd] :: tail
          else
            hd :: newVector :: tail
      }
    else
      (* In middle of vector. *)
      let
        val length = idx - prevIdx
        val slice1 = VectorSlice.slice (hd, 0, SOME length)
        val slice2 = VectorSlice.slice (hd, length, SOME
          (Vector.length hd - length))
      in
        if isThreeSliceLessThanTarget (slice1, slice2, newVector) then
          let
            val newVector = VectorSlice.full newVector
            val hd = VectorSlice.concat [slice1, newVector, slice2]
          in
            { idx = curIdx + VectorSlice.length newVector
            , left = hd :: tail
            , right = right
            }
          end
        else if isSliceLessThanTarget (slice1, newVector) then
          let
            val idx =
              prevIdx + VectorSlice.length slice1 + Vector.length newVector
            val newVector = VectorSlice.full newVector
            val lhd = VectorSlice.concat [slice1, newVector]
          in
            { idx = idx
            , left = joinEndOfLeft (lhd, tail)
            , right = joinStartOfRight (VectorSlice.vector slice2, right)
            }
          end
        else if isSliceLessThanTarget (slice2, newVector) then
          let
            val idx = prevIdx + VectorSlice.length slice1
            val newVector = VectorSlice.full newVector
            val rhd = VectorSlice.concat [newVector, slice2]
          in
            { idx = idx
            , left = joinEndOfLeft (VectorSlice.vector slice1, tail)
            , right = joinStartOfRight (rhd, right)
            }
          end
        else
          let
            val slice1 = VectorSlice.vector slice1
            val slice2 = VectorSlice.vector slice2
          in
            { idx = prevIdx
            , left = tail
            , right = slice1 :: newVector :: slice2 :: right
            }
          end
      end

  fun insRight (nextIdx, idx, newVector, curIdx, left, hd, tail) =
    if idx = nextIdx then
      (* At end of next Vector. *)
      { idx = curIdx
      , left = left
      , right =
          if isLessThanTarget (newVector, hd) then
            Vector.concat [hd, newVector] :: tail
          else
            hd :: (joinStartOfRight (newVector, tail))
      }
    else
      let
        val length = idx - curIdx
        val slice1 = VectorSlice.slice (hd, 0, SOME length)
        val slice2 = VectorSlice.slice (hd, length, SOME
          (Vector.length hd - length))
      in
        if isThreeSliceLessThanTarget (slice1, slice2, newVector) then
          let
            val idx =
              curIdx + VectorSlice.length slice1 + Vector.length newVector
              + VectorSlice.length slice2

            val newVector = VectorSlice.full newVector
            val lhd = VectorSlice.concat [slice1, newVector, slice2]
          in
            {idx = idx, left = joinEndOfLeft (lhd, left), right = tail}
          end
        else if isSliceLessThanTarget (slice1, newVector) then
          let
            val idx =
              curIdx + VectorSlice.length slice1 + Vector.length newVector
            val lhd = VectorSlice.concat [slice1, VectorSlice.full newVector]
          in
            { idx = idx
            , left = joinEndOfLeft (lhd, left)
            , right = joinStartOfRight (VectorSlice.vector slice2, tail)
            }
          end
        else if isSliceLessThanTarget (slice2, newVector) then
          let
            val idx = curIdx + VectorSlice.length slice1
            val lhd = VectorSlice.vector slice1
            val newVector = VectorSlice.full newVector
            val rhd = VectorSlice.concat [newVector, slice2]
          in
            { idx = idx
            , left = joinEndOfLeft (lhd, left)
            , right = joinStartOfRight (rhd, tail)
            }
          end
        else
          let
            val idx =
              curIdx + VectorSlice.length slice1 + Vector.length newVector
            val slice1 = VectorSlice.vector slice1
            val slice2 = VectorSlice.vector slice2
          in
            { idx = idx
            , left = newVector :: joinEndOfLeft (slice1, left)
            , right = joinStartOfRight (slice2, tail)
            }
          end
      end

  fun ins (idx, newVector, curIdx, left, right) : t =
    if curIdx = idx then
      preferInsertLeft (curIdx, newVector, left, right)
    else if idx < curIdx then
      (* Need to insert on the left. *)
      case left of
        [] =>
          (* If there is no vector on the left, then add the new vector there. *)
          {idx = Vector.length newVector, left = [newVector], right = right}
      | hd :: tail =>
          let
            val prevIdx = curIdx - Vector.length hd
          in
            if idx < prevIdx then
              (* The requested index is prior to the vector on the left,
               * so move leftward one vector. *)
              ins (idx, newVector, prevIdx, tail, joinStartOfRight (hd, right))
            else
              insLeft (prevIdx, idx, newVector, curIdx, hd, tail, right)
          end
    else
      (* Need to insert to the right. *)
      case right of
        [] => {idx = curIdx, left = left, right = [newVector]}
      | hd :: tail =>
          let
            val nextIdx = Vector.length hd + curIdx
          in
            if idx > nextIdx then
              ins (idx, newVector, nextIdx, joinEndOfLeft (hd, left), tail)
            else
              insRight (nextIdx, idx, newVector, curIdx, left, hd, tail)
          end

  fun insertMany (idx, newVector, buffer: t) =
    ins (idx, newVector, #idx buffer, #left buffer, #right buffer)

  fun insert (idx, elem, buffer) =
    insertMany (idx, Vector.fromList [elem], buffer)

  fun deleteRightFromHere (curIdx, finish, right) =
    case right of
      hd :: tail =>
        let
          val nextIdx = curIdx + Vector.length hd
        in
          if nextIdx < finish then
            deleteRightFromHere (nextIdx, finish, tail)
          else if nextIdx > finish then
            let
              val newVecStart = finish - curIdx
              val slice = VectorSlice.slice (hd, newVecStart, SOME
                (Vector.length hd - newVecStart))
              val newVec = VectorSlice.vector slice
            in
              newVec :: tail
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
          val nextIdx = curIdx + Vector.length hd
        in
          if nextIdx < start then
            (* Keep moving right: haven't reached start yet. *)
            moveRightAndDelete
              (start, finish, nextIdx, joinEndOfLeft (hd, left), tail)
          else if nextIdx > start then
            if nextIdx < finish then
              (* Delete the start range contained in this vector,
               * and then continue deleting right. *)
              let
                val length = start - curIdx
                val newVector = VectorSlice.slice (hd, 0, SOME length)
                val newVector = VectorSlice.vector newVector
              in
                { idx = curIdx + Vector.length newVector
                , left = joinEndOfLeft (newVector, left)
                , right = deleteRightFromHere (nextIdx, finish, tail)
                }
              end
            else if nextIdx > finish then
              (* Have to delete from middle of vector. *)
              let
                val sub1Length = start - curIdx
                val sub2Start = finish - curIdx
                val sub2Len = Vector.length hd - sub2Start

                val slice1 = VectorSlice.slice (hd, 0, SOME sub1Length)
                val slice2 = VectorSlice.slice (hd, sub2Start, SOME sub2Len)
                val slice1 = VectorSlice.vector slice1
                val slice2 = VectorSlice.vector slice2
              in
                { idx = curIdx + sub1Length
                , left = joinEndOfLeft (slice1, left)
                , right = joinStartOfRight (slice2, tail)
                }
              end
            else
              (* nextIdx = finish 
               * Have to delete from end of this vector. *)
              let
                val vecLength = start - curIdx
                val vec = VectorSlice.slice (hd, 0, SOME vecLength)
                val vec = VectorSlice.vector vec
              in
                { idx = curIdx + vecLength
                , left = joinEndOfLeft (vec, left)
                , right = tail
                }
              end
          else
            (* nextIdx = start
             * The start range is contained fully at the next node,
             * without having to remove part of a vector at this node.*)
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
          val prevIdx = curIdx - Vector.length hd
        in
          if start < prevIdx then
            deleteLeftFromHere (start, prevIdx, tail, right)
          else if start > prevIdx then
            (* Need to delete from some part of this vector. *)
            let
              val length = start - prevIdx
              val newVec = VectorSlice.slice (hd, 0, SOME length)
              val newVec = VectorSlice.vector newVec
            in
              { idx = prevIdx
              , left = tail
              , right = joinStartOfRight (newVec, right)
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
          val prevIdx = curIdx - Vector.length hd
        in
          if prevIdx > finish then
            moveLeftAndDelete
              (start, finish, prevIdx, tail, joinStartOfRight (hd, right))
          else if prevIdx < finish then
            if prevIdx > start then
              (* Delete from start point of this vector, 
               * and then call function to continue deleting leftward. *)
              let
                val hdStart = finish - prevIdx
                val newLen = Vector.length hd - hdStart
                val newHd = VectorSlice.slice (hd, hdStart, SOME newLen)
                val newHd = VectorSlice.vector newHd

                val right = joinStartOfRight (newHd, right)
              in
                deleteLeftFromHere (start, prevIdx, tail, right)
              end
            else if prevIdx < start then
              (* We want to delete in the middle of this current vector. *)
              let
                val sub1Length = start - prevIdx
                val sub2Start = finish - prevIdx
                val sub2Len = Vector.length hd - sub2Start

                val slice1 = VectorSlice.slice (hd, 0, SOME sub1Length)
                val slice2 = VectorSlice.slice (hd, sub2Start, SOME sub2Len)
                val slice1 = VectorSlice.vector slice1
                val slice2 = VectorSlice.vector slice2
              in
                { idx = prevIdx + sub1Length
                , left = joinEndOfLeft (slice1, tail)
                , right = joinStartOfRight (slice2, right)
                }
              end
            else
              (* prevIdx = start 
               * We want to delete from the start of this vector and stop. *)
              let
                val vecStart = finish - prevIdx
                val vecLen = Vector.length hd - vecStart
                val vec = VectorSlice.slice (hd, vecStart, SOME vecLen)
                val vec = VectorSlice.vector vec
              in
                { idx = prevIdx
                , left = tail
                , right = joinStartOfRight (vec, right)
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

  fun deleteMany (start, length, buffer: t) =
    if length > 0 then
      del (start, start + length, #idx buffer, #left buffer, #right buffer)
    else
      buffer
end
