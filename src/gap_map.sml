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

  type key = Fn.key
  type value = Fn.value

  type t =
    { leftKeys: Fn.key vector list
    , leftVals: Fn.value vector list
    , rightKeys: Fn.key vector list
    , rightVals: Fn.value vector list
    }

  val empty: t
  val isEmpty: t -> bool

  val add: Fn.key * Fn.value * t -> t
  val remove: Fn.key * t -> t

  val get: Fn.key * t -> Fn.value option

  val moveToStart: t -> t
  val moveToEnd: t -> t
  val moveTo: Fn.key * t -> t
end

signature MAP =
sig
  structure Pair: GAP_MAP_PAIR
  type env
  val map: Pair.value * env -> Pair.value
end

signature MAPPER =
sig
  structure Map: MAP
  structure Pair: GAP_MAP_PAIR

  type t =
    { leftKeys: Pair.key vector list
    , leftVals: Pair.value vector list
    , rightKeys: Pair.key vector list
    , rightVals: Pair.value vector list
    }

  val map: t * Map.env -> t
end

functor MakeGapMapMapper(Map: MAP): MAPPER =
struct
  structure Map = Map
  structure Pair = Map.Pair

  type t =
    { leftKeys: Pair.key vector list
    , leftVals: Pair.value vector list
    , rightKeys: Pair.key vector list
    , rightVals: Pair.value vector list
    }

  fun mapList (hd :: tl, acc, env) =
        let val hd = Vector.map (fn el => Map.map (el, env)) hd
        in mapList (tl, hd :: acc, env)
        end
    | mapList ([], acc, env) = List.rev acc

  fun map ({leftKeys, leftVals, rightKeys, rightVals}: t, env) : t =
    let
      val leftVals = mapList (leftVals, [], env)
      val rightVals = mapList (rightVals, [], env)
    in
      { leftKeys = leftKeys
      , leftVals = leftVals
      , rightKeys = rightKeys
      , rightVals = rightVals
      }
    end
end

functor MakeGapMap(Fn: GAP_MAP_PAIR): GAP_MAP =
struct
  structure Fn = Fn

  type key = Fn.key
  type value = Fn.value

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
                , rightVals = rightVals
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
                , rightVals = rightVals
                }
              end
          end
        else
          let
            val leftKeys = VectorSlice.vector lkhd :: leftKeys
            val leftVals = VectorSlice.vector lvhd :: leftVals
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
                , rightVals = rightVals
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
                , rightVals = rightVals
                }
              end
          end
    | _ =>
        let
          val lkhd = VectorSlice.vector lkhd
          val rkhd = VectorSlice.vector rkhd
          val lvhd = VectorSlice.vector lvhd
          val rvhd = VectorSlice.vector rvhd

          val leftKeys = lkhd :: leftKeys
          val leftVals = lvhd :: leftVals
          val rightKeys = rkhd :: rightKeys
          val rightVals = rvhd :: rightVals
        in
          { leftKeys = leftKeys
          , leftVals = leftVals
          , rightKeys = rightKeys
          , rightVals = rightVals
          }
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
        val rvhd = VectorSlice.concat [VectorSlice.full newVal, rvhd]
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

  fun insLeft (newKey, newVal, leftKeys, leftVals, rightKeys, rightVals) =
    case (leftKeys, leftVals) of
      (lkhd :: lktl, lvhd :: lvtl) =>
        let
          val insPos = findInsPos (newKey, lkhd)
        in
          if insPos = ~1 then
            (* move leftwards, joining hd with right if possible *)
            (case (rightKeys, rightVals) of
               (rkhd :: rktl, rvhd :: rvtl) =>
                 if isLessThanTarget (lkhd, rkhd) then
                   let
                     val rightKeys = Vector.concat [lkhd, rkhd] :: rktl
                     val rightVals = Vector.concat [lvhd, rvhd] :: rvtl
                   in
                     insLeft (newKey, newVal, lktl, lvtl, rightKeys, rightVals)
                   end
                 else
                   insLeft
                     ( newKey
                     , newVal
                     , lktl
                     , lvtl
                     , lkhd :: rightKeys
                     , lvhd :: rightVals
                     )
             | (_, _) =>
                 insLeft
                   ( newKey
                   , newVal
                   , lktl
                   , lvtl
                   , lkhd :: rightKeys
                   , lvhd :: rightVals
                   ))
          else if insPos = Vector.length lkhd then
            (* insert at end *)
            if Vector.length lkhd + 1 > Fn.maxNodeSize then
              let
                val hdKeys = Vector.fromList [newKey]
                val hdVals = Vector.fromList [newVal]
              in
                tryJoinStartOfRight
                  (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals)
              end
            else
              let
                (* join to end without splitting *)
                val leftKeys =
                  Vector.concat [lkhd, Vector.fromList [newKey]] :: lktl
                val leftVals =
                  Vector.concat [lvhd, Vector.fromList [newVal]] :: lvtl
              in
                { leftKeys = leftKeys
                , leftVals = leftVals
                , rightKeys = rightKeys
                , rightVals = rightVals
                }
              end
          else
            insMiddle
              ( lkhd
              , lvhd
              , insPos
              , newKey
              , newVal
              , lktl
              , lvtl
              , rightKeys
              , rightVals
              )
        end
    | (_, _) =>
        let
          val hdKeys = Vector.fromList [newKey]
          val hdVals = Vector.fromList [newVal]
        in
          tryJoinStartOfRight
            (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals)
        end

  fun insRight (newKey, newVal, leftKeys, leftVals, rightKeys, rightVals) =
    case (rightKeys, rightVals) of
      (rkhd :: rktl, rvhd :: rvtl) =>
        let
          val insPos = findInsPos (newKey, rkhd)
        in
          if insPos = Vector.length rkhd then
            (* move right, joining if possible while staying under maxNodeSize *)
            (case (leftKeys, leftVals) of
               (lkhd :: lktl, lvhd :: lvtl) =>
                 if isLessThanTarget (lkhd, rkhd) then
                   let
                     val leftKeys = Vector.concat [lkhd, rkhd] :: lktl
                     val leftVals = Vector.concat [lvhd, rvhd] :: lvtl
                   in
                     insRight (newKey, newVal, leftKeys, leftVals, rktl, rvtl)
                   end
                 else
                   let
                     val leftKeys = rkhd :: leftKeys
                     val leftVals = rvhd :: leftVals
                   in
                     insRight (newKey, newVal, leftKeys, leftVals, rktl, rvtl)
                   end
             | (_, _) =>
                 let
                   val leftKeys = rkhd :: leftKeys
                   val leftVals = rvhd :: leftVals
                 in
                   insRight (newKey, newVal, leftKeys, leftVals, rktl, rvtl)
                 end)
          else if insPos < 0 then
            (* insert at start *)
            if Vector.length rkhd + 1 > Fn.maxNodeSize then
              let
                (* hd is full so split *)
                val hdKeys = Vector.fromList [newKey]
                val hdVals = Vector.fromList [newVal]
              in
                tryJoinMaxSide
                  (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals)
              end
            else
              let
                (* join to start without splitting *)
                val rightKeys =
                  Vector.concat [Vector.fromList [newKey], rkhd] :: rktl
                val rightVals =
                  Vector.concat [Vector.fromList [newVal], rvhd] :: rvtl
              in
                { leftKeys = leftKeys
                , leftVals = leftVals
                , rightKeys = rightKeys
                , rightVals = rightVals
                }
              end
          else
            insMiddle
              ( rkhd
              , rvhd
              , insPos
              , newKey
              , newVal
              , leftKeys
              , leftVals
              , rktl
              , rvtl
              )
        end
    | (_, _) =>
        let
          val hdKeys = Vector.fromList [newKey]
          val hdVals = Vector.fromList [newVal]
        in
          tryJoinMaxSide
            (hdKeys, hdVals, leftKeys, leftVals, rightKeys, rightVals)
        end

  fun add (newKey, newVal, {leftKeys, leftVals, rightKeys, rightVals}) =
    (* look at elements to see which way to traverse *)
    case rightKeys of
      hd :: _ =>
        let
          val rfirst = Vector.sub (hd, 0)
        in
          if Fn.l (newKey, rfirst) then
            insLeft (newKey, newVal, leftKeys, leftVals, rightKeys, rightVals)
          else
            insRight (newKey, newVal, leftKeys, leftVals, rightKeys, rightVals)
        end
    | [] => insLeft (newKey, newVal, leftKeys, leftVals, rightKeys, rightVals)

  fun getLeft (check, leftKeys, leftVals) =
    case (leftKeys, leftVals) of
      (lkhd :: lktl, lvhd :: lvtl) =>
        let
          val pos = findInsPos (check, lkhd)
        in
          if pos < 0 then
            getLeft (check, lktl, lvtl)
          else if pos = Vector.length lkhd then
            NONE
          else
            let
              val posEl = Vector.sub (lkhd, pos)
            in
              if Fn.eq (check, posEl) then SOME (Vector.sub (lvhd, pos))
              else NONE
            end
        end
    | (_, _) => NONE

  fun getRight (check, rightKeys, rightVals) =
    case (rightKeys, rightVals) of
      (rkhd :: rktl, rvhd :: rvtl) =>
        let
          val pos = findInsPos (check, rkhd)
        in
          if pos = Vector.length rkhd then
            getRight (check, rktl, rvtl)
          else if pos < 0 then
            NONE
          else
            let
              val posEl = Vector.sub (rkhd, pos)
            in
              if Fn.eq (check, posEl) then SOME (Vector.sub (rvhd, pos))
              else NONE
            end
        end
    | (_, _) => NONE

  fun get (check, {leftKeys, leftVals, rightKeys, rightVals}) =
    case (rightKeys, rightVals) of
      (rkhd :: _, rvhd :: _) =>
        let
          val first = Vector.sub (rkhd, 0)
        in
          if Fn.g (check, first) then getRight (check, rightKeys, rightVals)
          else if Fn.eq (check, first) then SOME (Vector.sub (rvhd, 0))
          else getLeft (check, leftKeys, leftVals)
        end
    | (_, _) => getLeft (check, leftKeys, leftVals)

  fun helpMoveToStart (leftKeys, leftVals, rightKeys, rightVals) =
    case (leftKeys, leftVals) of
      (lkhd :: lktl, lvhd :: lvtl) =>
        (case (rightKeys, rightVals) of
           (rkhd :: rktl, rvhd :: rvtl) =>
             if isLessThanTarget (lkhd, rvhd) then
               let
                 val rightKeys = Vector.concat [lkhd, rkhd] :: rktl
                 val rightVals = Vector.concat [lvhd, rvhd] :: rvtl
               in
                 helpMoveToStart (lktl, lvtl, rightKeys, rightVals)
               end
             else
               let
                 val rightKeys = lkhd :: rightKeys
                 val rightVals = lvhd :: rightVals
               in
                 helpMoveToStart (lktl, lvtl, rightKeys, rightVals)
               end
         | (_, _) =>
             let
               val rightKeys = lkhd :: rightKeys
               val rightVals = lvhd :: rightVals
             in
               helpMoveToStart (lktl, lvtl, rightKeys, rightVals)
             end)
    | (_, _) =>
        { leftKeys = leftKeys
        , leftVals = leftVals
        , rightKeys = rightKeys
        , rightVals = rightVals
        }

  fun moveToStart {leftKeys, leftVals, rightKeys, rightVals} =
    helpMoveToStart (leftKeys, leftVals, rightKeys, rightVals)

  fun helpMoveToEnd (leftKeys, leftVals, rightKeys, rightVals) =
    case (rightKeys, rightVals) of
      (rkhd :: rktl, rvhd :: rvtl) =>
        (case (leftKeys, leftVals) of
           (lkhd :: lktl, lvhd :: lvtl) =>
             if isLessThanTarget (lkhd, rkhd) then
               let
                 val leftKeys = Vector.concat [lkhd, rkhd] :: leftKeys
                 val leftVals = Vector.concat [lvhd, rvhd] :: leftVals
               in
                 helpMoveToEnd (leftKeys, leftVals, rktl, rvtl)
               end
             else
               let
                 val leftKeys = rkhd :: leftKeys
                 val leftVals = rvhd :: leftVals
               in
                 helpMoveToEnd (leftKeys, leftVals, rktl, rvtl)
               end
         | (_, _) =>
             let
               val leftKeys = rkhd :: leftKeys
               val leftVals = rvhd :: leftVals
             in
               helpMoveToEnd (leftKeys, leftVals, rktl, rvtl)
             end)
    | (_, _) =>
        { leftKeys = leftKeys
        , leftVals = leftVals
        , rightKeys = rightKeys
        , rightVals = rightVals
        }

  fun moveToEnd {leftKeys, leftVals, rightKeys, rightVals} =
    helpMoveToEnd (leftKeys, leftVals, rightKeys, rightVals)

  fun moveLeft (to, leftKeys, leftVals, rightKeys, rightVals) =
    case (leftKeys, leftVals) of
      (lkhd :: lktl, lvhd :: lvtl) =>
        let
          val first = Vector.sub (lkhd, 0)
        in
          if Fn.l (to, first) then
            (case (rightKeys, rightVals) of
               (rkhd :: rktl, rvhd :: rvtl) =>
                 if isLessThanTarget (lkhd, rkhd) then
                   let
                     val rightKeys = Vector.concat [lkhd, rkhd] :: rktl
                     val rightVals = Vector.concat [lvhd, rvhd] :: rvtl
                   in
                     moveLeft (to, lktl, lvtl, rightKeys, rightVals)
                   end
                 else
                   let
                     val rightKeys = lkhd :: rightKeys
                     val rightVals = lvhd :: rightVals
                   in
                     moveLeft (to, lktl, lvtl, rightKeys, rightVals)
                   end
             | (_, _) =>
                 let
                   val rightKeys = lkhd :: rightKeys
                   val rightVals = lvhd :: rightVals
                 in
                   moveLeft (to, lktl, lvtl, rightKeys, rightVals)
                 end)
          else
            { leftKeys = leftKeys
            , leftVals = leftVals
            , rightKeys = rightKeys
            , rightVals = rightVals
            }
        end
    | (_, _) =>
        { leftKeys = leftKeys
        , leftVals = leftVals
        , rightKeys = rightKeys
        , rightVals = rightVals
        }

  fun moveRight (to, leftKeys, leftVals, rightKeys, rightVals) =
    case (rightKeys, rightVals) of
      (rkhd :: rktl, rvhd :: rvtl) =>
        let
          val last = Vector.sub (rkhd, Vector.length rkhd - 1)
        in
          if Fn.g (to, last) then
            (case (leftKeys, leftVals) of
               (lkhd :: lktl, lvhd :: lvtl) =>
                 if isLessThanTarget (lkhd, rkhd) then
                   let
                     val leftKeys = Vector.concat [lkhd, rkhd] :: lktl
                     val leftVals = Vector.concat [lvhd, rvhd] :: lvtl
                   in
                     moveRight (to, leftKeys, leftVals, rktl, rvtl)
                   end
                 else
                   let
                     val leftKeys = rkhd :: leftKeys
                     val leftVals = rvhd :: leftVals
                   in
                     moveRight (to, leftKeys, leftVals, rktl, rvtl)
                   end
             | (_, _) =>
                 let
                   val leftKeys = rkhd :: leftKeys
                   val leftVals = rvhd :: leftVals
                 in
                   moveRight (to, leftKeys, leftVals, rktl, rvtl)
                 end)
          else
            { leftKeys = leftKeys
            , leftVals = leftVals
            , rightKeys = rightKeys
            , rightVals = rightVals
            }
        end
    | (_, _) =>
        { leftKeys = leftKeys
        , leftVals = leftVals
        , rightKeys = rightKeys
        , rightVals = rightVals
        }

  fun moveToWhenRightIsEmpty
    (to, map as {leftKeys, leftVals, rightKeys, rightVals}) =
    case leftKeys of
      hd :: _ =>
        let
          val llast = Vector.sub (hd, Vector.length hd - 1)
        in
          if Fn.l (to, llast) then
            moveLeft (to, leftKeys, leftVals, rightKeys, rightVals)
          else
            map
        end
    | [] => map

  fun moveTo (to, map as {leftKeys, leftVals, rightKeys, rightVals}) =
    case rightKeys of
      hd :: _ =>
        let
          val rfirst = Vector.sub (hd, 0)
        in
          if Fn.l (to, rfirst) then
            moveLeft (to, leftKeys, leftVals, rightKeys, rightVals)
          else
            moveRight (to, leftKeys, leftVals, rightKeys, rightVals)
        end
    | [] => moveToWhenRightIsEmpty (to, map)

  fun removeMiddle (khd, vhd, insPos, leftKeys, leftVals, rightKeys, rightVals) =
    let
      val rLen = Vector.length khd - insPos
      val lkhd = VectorSlice.slice (khd, 0, SOME insPos)
      val rkhd = VectorSlice.slice (khd, insPos, SOME rLen)

      val lvhd = VectorSlice.slice (vhd, 0, SOME insPos)
      val rvhd = VectorSlice.slice (vhd, insPos, SOME rLen)

      val khd = VectorSlice.concat [lkhd, rkhd]
      val vhd = VectorSlice.concat [lvhd, rvhd]
    in
      tryJoinMaxSide (khd, vhd, leftKeys, leftVals, rightKeys, rightVals)
    end

  fun removeLeft (toRemove, leftKeys, leftVals, rightKeys, rightVals) =
    case (leftKeys, leftVals) of
      (lkhd :: lktl, lvhd :: lvtl) =>
        let
          val insPos = findInsPos (toRemove, lkhd)
        in
          if insPos < 0 then
            (* keep moving left, joining if possible *)
            (case (rightKeys, rightVals) of
               (rkhd :: rktl, rvhd :: rvtl) =>
                 if isLessThanTarget (lkhd, rkhd) then
                   let
                     val rightKeys = Vector.concat [lkhd, rkhd] :: rktl
                     val rightVals = Vector.concat [lvhd, rvhd] :: rvtl
                   in
                     removeLeft (toRemove, lktl, lvtl, rightKeys, rightVals)
                   end
                 else
                   let
                     val rightKeys = lkhd :: rightKeys
                     val rightVals = lvhd :: rightVals
                   in
                     removeLeft (toRemove, lktl, lvtl, rightKeys, rightVals)
                   end
             | (_, _) =>
                 let
                   val rightKeys = lkhd :: rightKeys
                   val rightVals = lvhd :: rightVals
                 in
                   removeLeft (toRemove, lktl, lvtl, rightKeys, rightVals)
                 end)
          else if insPos = Vector.length lkhd then
            { leftKeys = leftKeys
            , leftVals = leftVals
            , rightKeys = rightKeys
            , rightVals = rightVals
            }
          else if Fn.eq (toRemove, Vector.sub (lkhd, insPos)) then
            (* found key so remove key/value pair *)
            removeMiddle (lkhd, lvhd, insPos, lktl, lvtl, rightKeys, rightVals)
          else
            (* not found so just return *)
            { leftKeys = leftKeys
            , leftVals = leftVals
            , rightKeys = rightKeys
            , rightVals = rightVals
            }
        end
    | (_, _) =>
        { leftKeys = leftKeys
        , leftVals = leftVals
        , rightKeys = rightKeys
        , rightVals = rightVals
        }

  fun removeRight (toRemove, leftKeys, leftVals, rightKeys, rightVals) =
    case (rightKeys, rightVals) of
      (rkhd :: rktl, rvhd :: rvtl) =>
        let
          val insPos = findInsPos (toRemove, rkhd)
        in
          if insPos = Vector.length rkhd then
            (case (leftKeys, leftVals) of
               (lkhd :: lktl, lvhd :: lvtl) =>
                 if isLessThanTarget (lkhd, rkhd) then
                   let
                     val leftKeys = Vector.concat [lkhd, rkhd] :: lktl
                     val leftVals = Vector.concat [lvhd, rvhd] :: lvtl
                   in
                     removeRight (toRemove, leftKeys, leftVals, rktl, rvtl)
                   end
                 else
                   let
                     val leftKeys = rkhd :: leftKeys
                     val leftVals = rvhd :: leftVals
                   in
                     removeRight (toRemove, leftKeys, leftVals, rktl, rvtl)
                   end
             | (_, _) =>
                 let
                   val leftKeys = rkhd :: leftKeys
                   val leftVals = rvhd :: leftVals
                 in
                   removeRight (toRemove, leftKeys, leftVals, rktl, rvtl)
                 end)
          else if insPos < 0 then
            { leftKeys = leftKeys
            , leftVals = leftVals
            , rightKeys = rightKeys
            , rightVals = rightVals
            }
          else if Fn.eq (toRemove, Vector.sub (rkhd, insPos)) then
            removeMiddle (rkhd, rvhd, insPos, leftKeys, leftVals, rktl, rvtl)
          else
            { leftKeys = leftKeys
            , leftVals = leftVals
            , rightKeys = rightKeys
            , rightVals = rightVals
            }
        end
    | (_, _) =>
        { leftKeys = leftKeys
        , leftVals = leftVals
        , rightKeys = rightKeys
        , rightVals = rightVals
        }

  fun remove (toRemove, {leftKeys, leftVals, rightKeys, rightVals}) =
    case rightKeys of
      hd :: _ =>
        let
          val rfirst = Vector.sub (hd, 0)
        in
          if Fn.l (toRemove, rfirst) then
            removeLeft (toRemove, leftKeys, leftVals, rightKeys, rightVals)
          else
            removeRight (toRemove, leftKeys, leftVals, rightKeys, rightVals)
        end
    | _ => removeLeft (toRemove, leftKeys, leftVals, rightKeys, rightVals)
end (* example usage of functor to map over GapMap:
    structure Pair =
    struct
      type key = int
      type value = int
    
      fun l (a: int, b: int) = a < b
      fun eq (a: int, b: int) = a = b
      fun g (a: int, b: int) = a > b
    
      val maxNodeSize = 1024
    end
    
    structure IntPair = MakeGapMap(Pair)
    
    structure IntMap =
      MakeGapMapMapper
        (struct
           structure Pair = Pair
           type env = ()
           fun map x () = x * 5
         end)
    *)
