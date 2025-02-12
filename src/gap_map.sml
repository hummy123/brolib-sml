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
    , leftVals: Fn.value vector list
    , rightKeys: Fn.key vector list
    , rightVals: Fn.value vector list
    }

  val empty = {leftKeys = [], leftVals = [], rightKeys = [], rightVals = []}

  fun isEmpty ({leftKeys = [], rightKeys = [], ...}: t) = true
    | isEmpty _ = false

  fun isLessThanTarget (v1, v2) =
    Vector.length v1 + Vector.length v2 <= Fn.maxNodeSize

  fun reverseLinearSearch (pos, findNum, vec) =
    if pos < 0 then
      ~1
    else
      let
        val curNum = Vector.sub (vec, pos)
      in
        if Fn.l (findNum, curNum) then pos
        else reverseLinearSearch (pos - 1, findNum, vec)
      end

  fun forwardLinearSearch (pos, findNum, vec) =
    if pos = Vector.length vec then
      Vector.length vec
    else
      let
        val curNum = Vector.sub (vec, pos)
      in
        if Fn.g (findNum, curNum) then pos + 1
        else forwardLinearSearch (pos + 1, findNum, vec)
      end

  fun helpFindInsPos (findNum, vec, low, high, prevMid) =
    if high >= low then
      let
        val mid = low + ((high - low) div 2)
        val curNum = Vector.sub (vec, mid)
      in
        if Fn.eq (curNum, findNum) then
          mid
        else if Fn.l (curNum, findNum) then
          helpFindInsPos (findNum, vec, mid + 1, high, mid)
        else
          helpFindInsPos (findNum, vec, low, mid - 1, mid)
      end
    else
      let
        val curNum = Vector.sub (vec, prevMid)
      in
        if Fn.g (findNum, curNum) then
          forwardLinearSearch (prevMid, findNum, vec)
        else
          reverseLinearSearch (prevMid, findNum, vec)
      end

  fun findInsPos (findNum, vec) =
    if Vector.length vec = 0 then ~1
    else helpFindInsPos (findNum, vec, 0, Vector.length vec - 1, 0)

  fun insWithPos (vec, elem, insPos) =
    if insPos < 0 then
      Vector.concat [Vector.fromList [elem], vec]
    else if insPos = Vector.length vec then
      Vector.concat [vec, Vector.fromList [elem]]
    else
      let
        val elem = Vector.fromList [elem]
        val elem = VectorSlice.full elem

        val s2len = Vector.length vec - insPos
        val slice1 = VectorSlice.slice (vec, 0, SOME insPos)
        val slice2 = VectorSlice.slice (vec, insPos, SOME s2len)
      in
        VectorSlice.concat [slice1, elem, slice2]
      end

  fun consLeft (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals) =
    let
      val leftKeys = hdKeys :: leftKeys
      val leftVals = hdVals :: leftVals
    in
      { leftKeys = leftKeys
      , leftVals = leftVals
      , rightKeys = rightKeys
      , rightVals = rightVals
      }
    end

  fun concatLeft (hdKeys, hdVals, lkhd, lktl, lvhd, lvtl, rightKeys, rightVals) =
    let
      val leftKeys = Vector.concat [lkhd, hdKeys] :: lktl
      val leftVals = Vector.concat [lvhd, hdVals] :: lvtl
    in
      { leftKeys = leftKeys
      , leftVals = leftVals
      , rightKeys = rightKeys
      , rightVals = rightVals
      }
    end

  fun consRight (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals) =
    let
      val rightKeys = hdKeys :: rightKeys
      val rightVals = hdVals :: rightVals
    in
      { leftKeys = leftKeys
      , leftVals = leftVals
      , rightKeys = rightKeys
      , rightVals = rightVals
      }
    end

  fun concatRight (hdKeys, hdVals, leftKeys, leftVals, rkhd, rktl, rvhd, rvtl) =
    let
      val rightKeys = Vector.concat [hdKeys, rkhd] :: rktl
      val rightVals = Vector.concat [hdVals, rvhd] :: rvtl
    in
      { leftKeys = leftKeys
      , leftVals = leftVals
      , rightKeys = rightKeys
      , rightVals = rightVals
      }
    end

  fun tryJoinStartOfRight
    (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals) =
    case (rightKeys, rightVals) of
      (rkhd :: rktl, rvhd :: rvtl) =>
        if isLessThanTarget (rkhd, hdVals) then
          (* join to right *)
          concatRight
            (hdKeys, hdVals, leftKeys, leftVals, rkhd, rktl, rvhd, rvtl)
        else
          (* cannot join to left or right while staying in limit
           * so cons instead *)
          consRight (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals)
    | (_, _) =>
        consRight (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals)


  fun tryJoinMaxSide (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals) =
    case (leftKeys, leftVals) of
      (lkhd :: lktl, lvhd :: lvtl) =>
        if isLessThanTarget (lkhd, hdVals) then
          (* join to left *)
          concatLeft
            (hdKeys, hdVals, lkhd, lktl, lvhd, lvtl, rightKeys, rightVals)
        else
          tryJoinStartOfRight
            (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals)
    | (_, _) =>
        tryJoinStartOfRight
          (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals)

  fun isSliceLessThanTarget (v1, v2) =
    VectorSlice.length v1 + Vector.length v2 <= Fn.maxNodeSize

  fun joinSlices
    (lkhd, lvhd, rkhd, rvhd, leftKeys, leftVals, rightKeys, rightVals) =
    case (leftKeys, leftVals, rightKeys, rightVals) of
      (p_lkhd :: p_lktl, p_lvhd :: p_lvtl, p_rkhd :: p_rktl, p_rvhd :: p_rvtl) =>
        if isSliceLessThanTarget (lkhd, p_lkhd) then
          let
            val p_lkhd = VectorSlice.full p_lkhd
            val leftKeys = VectorSlice.concat [p_lkhd, lkhd] :: p_lktl
            val p_lvhd = VectorSlice.full p_lvhd
            val leftVals = VectorSlice.concat [p_lvhd, lvhd] :: p_lvtl
          in
            if isSliceLessThanTarget (rkhd, p_rkhd) then
              let
                val p_rkhd = VectorSlice.full p_rkhd
                val rightKeys = VectorSlice.concat [rkhd, p_rkhd] :: p_rktl
                val p_rvhd = VectorSlice.full p_rvhd
                val rightVals = VectorSlice.concat [rvhd, p_rvhd] :: p_rvtl
              in
                { leftKeys = leftKeys
                , leftVals = leftVals
                , rightKeys = rightKeys
                , rightVals = rightKeys
                }
              end
            else
              let
                val rightKeys = VectorSlice.vector rkhd :: rightKeys
                val rightVals = VectorSlice.vector rvhd :: rightVals
              in
                { leftKeys = leftKeys
                , leftVals = leftVals
                , rightKeys = rightKeys
                , rightVals = rightKeys
                }
              end
          end
        else
          let
            val leftKeys = VectorSlice.vector lkhd :: leftKeys
            val leftVals = VectorSlice.vector rvhd :: leftVals
          in
            if isSliceLessThanTarget (rkhd, p_rkhd) then
              let
                val p_rkhd = VectorSlice.full p_rkhd
                val rightKeys = VectorSlice.concat [rkhd, p_rkhd] :: p_rktl
                val p_rvhd = VectorSlice.full p_rvhd
                val rightVals = VectorSlice.concat [rvhd, p_rvhd] :: p_rvtl
              in
                { leftKeys = leftKeys
                , leftVals = leftVals
                , rightKeys = rightKeys
                , rightVals = rightKeys
                }
              end
            else
              let
                val rightKeys = VectorSlice.vector rkhd :: rightKeys
                val rightVals = VectorSlice.vector rvhd :: rightVals
              in
                { leftKeys = leftKeys
                , leftVals = leftVals
                , rightKeys = rightKeys
                , rightVals = rightKeys
                }
              end
          end

  fun insMiddle
    ( hdKeys
    , hdVals
    , insPos
    , newKey
    , newVal
    , leftKeys
    , leftVals
    , rightKeys
    , rightVals
    ) =
    (* insert in middle *)
    if Fn.eq (Vector.sub (hdKeys, insPos), newKey) then
      (* we already have this key so just update hdVals to have newVal 
       * at insPos *)
      let
        val hdVals =
          Vector.mapi (fn (idx, el) => if idx <> insPos then el else newVal)
            hdVals
      in
        tryJoinMaxSide
          (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals)
      end
    else if Vector.length hdKeys + 1 > Fn.maxNodeSize then
      let
        (* split into two vectors and join with new *)
        val lkhd = VectorSlice.slice (hdKeys, 0, SOME insPos)
        val lvhd = VectorSlice.slice (hdVals, 0, SOME insPos)

        val rhdLen = Vector.length hdKeys - insPos
        val rkhd = VectorSlice.slice (hdKeys, insPos, SOME rhdLen)
        val rvhd = VectorSlice.slice (hdVals, insPos, SOME rhdLen)

        (* add new key/new val to right *)
        val newKey = Vector.fromList [newKey]
        val rkhd = VectorSlice.concat [VectorSlice.full newKey, rkhd]
        val rkhd = VectorSlice.full rkhd

        val newVal = Vector.fromList [newVal]
        val rvhd = VectorSlice.concat [VectorSlice.full newVal, rkhd]
        val rvhd = VectorSlice.full rvhd
      in
        (* join both slices *)
        joinSlices
          (lkhd, lvhd, rkhd, rvhd, leftKeys, leftVals, rightKeys, rightVals)
      end
    else
      let
        (* insert without splitting *)
        val hdKeys = insWithPos (hdKeys, newKey, insPos)
        val hdVals = insWithPos (hdVals, newVal, insPos)
      in
        tryJoinMaxSide
          (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals)
      end
end
