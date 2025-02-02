signature GAP_SET_ELEMENT =
sig
  type key

  val l: key * key -> bool
  val eq: key * key -> bool
  val g: key * key -> bool

  val maxNodeSize: int
end

signature GAP_SET =
sig
  structure Fn: GAP_SET_ELEMENT

  type t

  val empty: t
  val isEmpty: t -> bool

  val add: Fn.key * t -> t
  val remove: Fn.key * t -> t

  val fromList: Fn.key list -> t
  val toVector: t -> Fn.key vector

  val exists: Fn.key * t -> bool
  val min: t -> Fn.key option
  val max: t -> Fn.key option

  val moveToStart: t -> t
  val moveToEnd: t -> t
  val moveTo: Fn.key * t -> t
end

functor MakeGapSet(Fn: GAP_SET_ELEMENT): GAP_SET =
struct
  structure Fn = Fn

  type t = {left: Fn.key vector list, right: Fn.key vector list}

  val empty = {left = [], right = []}

  fun isEmpty {left = [], right = []} = true
    | isEmpty _ = false

  fun singleton x =
    {left = [], right = Vector.fromList [x]}

  fun isLessThanTarget (v1, v2) =
    Vector.length v1 + Vector.length v2 <= Fn.maxNodeSize

  fun joinEndOfLeft (new, left) =
    case left of
      hd :: tail =>
        if isLessThanTarget (new, hd) then
          let val newHd = Vector.concat [hd, new]
          in newHd :: tail
          end
        else
          new :: left
    | [] => new :: left

  fun joinStartOfRight (new, right) =
    case right of
      hd :: tail =>
        if isLessThanTarget (new, hd) then
          let val newHd = Vector.concat [new, hd]
          in newHd :: tail
          end
        else
          new :: right
    | [] => new :: right

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

  fun insMiddle (hd, insPos, new, left, right) =
    (* insert in middle *)
    if Fn.eq (Vector.sub (hd, insPos), new) then
      (* already have this key so no need to insert again *)
      {left = left, right = right}
    else if Vector.length hd + 1 > Fn.maxNodeSize then
      let
        (* split into two vectors and join with new *)
        val lhd = VectorSlice.slice (hd, 0, SOME insPos)
        val rhdLen = Vector.length hd - insPos
        val rhd = VectorSlice.slice (hd, insPos, SOME rhdLen)

        val lhd = VectorSlice.vector lhd
        val new = Vector.fromList [new]
        val new = VectorSlice.full new
        val rhd = VectorSlice.concat [new, rhd]
      in
        {left = joinEndOfLeft (lhd, left), right = rhd :: right}
      end
    else
      let
        (* insert without splitting *)
        val newHd = insWithPos (hd, new, insPos)
      in
        {left = joinEndOfLeft (newHd, left), right = right}
      end

  fun insLeft (new, left, right) =
    case left of
      hd :: tl =>
        let
          val insPos = findInsPos (new, hd)
        in
          if insPos = ~1 then
            insLeft (new, tl, joinStartOfRight (hd, right))
          else if insPos = Vector.length hd then
            (* insert at end *)
            if Vector.length hd + 1 > Fn.maxNodeSize then
              let
                (* hd is full so join new to start of right *)
                val right = joinStartOfRight (Vector.fromList [new], right)
              in
                {left = left, right = right}
              end
            else
              let
                (* join to end without splitting *)
                val lhd = Vector.concat [hd, Vector.fromList [new]]
              in
                {left = joinEndOfLeft (lhd, tl), right = right}
              end
          else
            insMiddle (hd, insPos, new, left, right)
        end
    | [] =>
        let val new = Vector.fromList [new]
        in {left = left, right = joinStartOfRight (new, right)}
        end

  fun insRight (new, left, right) =
    case right of
      hd :: tl =>
        let
          val insPos = findInsPos (new, hd)
        in
          if insPos = Vector.length hd then
            insRight (new, joinEndOfLeft (hd, left), tl)
          else if insPos < 0 then
            (* insert at start *)
            if Vector.length hd + 1 > Fn.maxNodeSize then
              let
                (* hd is full so join new to end of left *)
                val left = joinEndOfLeft (Vector.fromList [new], left)
              in
                {left = left, right = right}
              end
            else
              let
                (* join to start without splitting *)
                val rhd = Vector.concat [Vector.fromList [new], hd]
              in
                {left = left, right = joinStartOfRight (rhd, tl)}
              end
          else
            insMiddle (hd, insPos, new, left, right)
        end
    | [] =>
        let val new = Vector.fromList [new]
        in {left = joinEndOfLeft (new, left), right = right}
        end

  fun add (new, {left, right}: t) =
    (* look at elements to see which way to traverse *)
    case right of
      hd :: _ =>
        let
          val rfist = Vector.sub (hd, 0)
        in
          if Fn.g (new, rfist) then insRight (new, left, right)
          else if Fn.l (new, rfist) then insLeft (new, left, right)
          else {left = left, right = right}
        end
    | [] => insLeft (new, left, right)

  fun helpMoveToStart (left, right) =
    case left of
      hd :: tl => helpMoveToStart (tl, joinStartOfRight (hd, right))
    | [] => {left = left, right = right}

  fun moveToStart {left, right} =
    case left of
      hd :: tl => helpMoveToStart (tl, joinStartOfRight (hd, right))
    | [] => {left = left, right = right}

  fun helpMoveToEnd (left, right) =
    case right of
      hd :: tl => helpMoveToEnd (joinEndOfLeft (hd, left), tl)
    | [] => {left = left, right = right}

  fun moveToEnd {left, right} =
    case right of
      hd :: tl => helpMoveToEnd (joinEndOfLeft (hd, left), tl)
    | [] => {left = left, right = right}

  fun moveLeft (to, left, right) =
    case left of
      hd :: tl =>
        let
          val first = Vector.sub (hd, 0)
        in
          if Fn.l (to, first) then
            moveLeft (to, tl, joinStartOfRight (hd, right))
          else
            {left = left, right = right}
        end
    | [] => {left = left, right = right}

  fun moveRight (to, left, right) =
    case right of
      hd :: tl =>
        let
          val last = Vector.sub (hd, Vector.length hd - 1)
        in
          if Fn.g (to, last) then moveRight (to, joinEndOfLeft (hd, left), tl)
          else {left = left, right = right}
        end
    | [] => {left = left, right = right}

  fun moveTo (to, {left, right}) =
    case right of
      hd :: _ =>
        let
          val rfist = Vector.sub (hd, 0)
        in
          if Fn.g (to, rfist) then moveRight (to, left, right)
          else if Fn.l (to, rfist) then moveLeft (to, left, right)
          else {left = left, right = right}
        end

  fun helpMin (hd :: tl, prevHd) = helpMin (tl, hd)
    | helpMin ([], prevHd) =
        SOME (Vector.sub (prevHd, 0))

  fun min {left = hd :: tl, right = _} = helpMin (tl, hd)
    | min {left = [], right = hd :: _} =
        SOME (Vector.sub (hd, 0))
    | min {left = [], right = []} = NONE

  fun helpMax (_, hd :: tl) = helpMax (hd, tl)
    | helpMax (hd, []) =
        SOME (Vector.sub (hd, Vector.length hd - 1))

  fun max {left = _, right = hd :: tl} = helpMax (hd, tl)
    | max {left = hd :: _, right = []} =
        SOME (Vector.sub (hd, Vector.length hd - 1))
    | max {left = [], right = []} = NONE

  fun existsLeft (check, hd :: tl) =
        let
          val pos = findInsPos (check, hd)
        in
          if pos < 0 then existsLeft (check, tl)
          else if pos = Vector.length hd then false
          else Fn.eq (Vector.sub (hd, pos), check)
        end
    | existsLeft (_, []) = false

  fun existsRight (check, hd :: tl) =
        let
          val pos = findInsPos (check, hd)
        in
          if pos = Vector.length hd then existsRight (check, tl)
          else if pos < 0 then false
          else Fn.eq (Vector.sub (hd, pos), check)
        end
    | existsRight (_, []) = false

  fun exists (check, {left, right}) =
    case right of
      hd :: tl =>
        let
          val first = Vector.sub (hd, 0)
        in
          if Fn.g (check, first) then existsRight (check, tl)
          else if Fn.eq (check, first) then true
          else existsLeft (check, left)
        end
    | [] => existsLeft (check, left)

  fun removeLeft (toRemove, left, right) =
    case left of
      hd :: tl =>
        let
          val insPos = findInsPos (toRemove, hd)
        in
          if insPos < 0 then
            removeLeft (toRemove, tl, joinStartOfRight (hd, right))
          else if insPos = Vector.length hd then
            {left = tl, right = joinStartOfRight (hd, right)}
          else if Fn.eq (toRemove, Vector.sub (hd, insPos)) then
            let
              val lhd = VectorSlice.slice (hd, 0, SOME insPos)
              val rhdLen = Vector.length hd - insPos
              val rhd = VectorSlice.slice (hd, insPos, SOME rhdLen)

              val lhd = VectorSlice.vector lhd
              val rhd = VectorSlice.vector rhd
            in
              { left = joinEndOfLeft (lhd, tl)
              , right = joinStartOfRight (rhd, right)
              }
            end
          else
            {left = tl, right = joinStartOfRight (hd, right)}
        end
    | [] => {left = left, right = right}

  fun removeRight (toRemove, left, right) =
    case right of
      hd :: tl =>
        let
          val insPos = findInsPos (toRemove, hd)
        in
          if insPos = Vector.length hd then
            removeRight (toRemove, joinEndOfLeft (hd, left), right)
          else if insPos < 0 then
            {left = joinEndOfLeft (hd, left), right = right}
          else if Fn.eq (toRemove, Vector.sub (hd, insPos)) then
            let
              val lhd = VectorSlice.slice (hd, 0, SOME insPos)
              val rhdLen = Vector.length hd - insPos
              val rhd = VectorSlice.slice (hd, insPos, SOME rhdLen)

              val lhd = VectorSlice.vector lhd
              val rhd = VectorSlice.vector rhd
            in
              { left = joinEndOfLeft (lhd, left)
              , right = joinStartOfRight (rhd, tl)
              }
            end
          else
            {left = joinEndOfLeft (hd, left), right = tl}
        end
    | [] => {left = left, right = right}

  fun remove (toRemove, {left, right}) =
    case right of
      hd :: tl =>
        let
          val rfirst = Vector.sub (hd, 0)
        in
          if Fn.g (toRemove, rfirst) then
            removeRight (toRemove, left, right)
          else if Fn.l (toRemove, rfirst) then
            removeLeft (toRemove, left, right)
          else
            let
              val len = Vector.length hd - 1
              val hd = VectorSlice.slice (hd, 1, SOME len)
              val hd = VectorSlice.vector hd
            in
              {left = left, right = joinStartOfRight (hd, tl)}
            end
        end
    | [] => removeLeft (toRemove, left, right)

  fun helpFromList (lst, acc) =
    case lst of
      hd :: tl => let val acc = add (hd, acc) in helpFromList (tl, acc) end
    | [] => acc

  fun fromList lst = helpFromList (lst, empty)

  fun helpToVector (hd :: tl, acc) =
        helpToVector (tl, hd :: acc)
    | helpToVector ([], acc) = Vector.concat acc

  fun toVector {left, right} = helpToVector (left, right)
end
