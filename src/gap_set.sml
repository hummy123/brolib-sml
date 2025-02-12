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

  val singleton: Fn.key -> t

  val add: Fn.key * t -> t
  val remove: Fn.key * t -> t
  val removeMany: Fn.key * Fn.key * t -> t

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
    {left = [], right = [Vector.fromList [x]]}

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
          val rfirst = Vector.sub (hd, 0)
        in
          if Fn.g (new, rfirst) then insRight (new, left, right)
          else if Fn.l (new, rfirst) then insLeft (new, left, right)
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

  fun moveToWhenRightIsEmpty (to, left, right) =
    case left of
      hd :: _ =>
        let
          val llast = Vector.sub (hd, Vector.length hd - 1)
        in
          if Fn.l (to, llast) then moveLeft (to, left, right)
          else {left = left, right = right}
        end
    | [] => {left = left, right = right}

  fun moveTo (to, {left, right}) =
    case right of
      hd :: _ =>
        let
          val rfirst = Vector.sub (hd, 0)
        in
          if Fn.g (to, rfirst) then moveRight (to, left, right)
          else if Fn.l (to, rfirst) then moveLeft (to, left, right)
          else {left = left, right = right}
        end
    | [] => moveToWhenRightIsEmpty (to, left, right)

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

  fun removeRightFromHere (finish, right) =
    case right of
      hd :: tl =>
        let
          val finishPos = findInsPos (finish, hd)
        in
          if finishPos = Vector.length hd then
            removeRightFromHere (finish, tl)
          else if finishPos < 0 then
            right
          else
            let
              (* keep second half of hd / remove first part of hd *)
              val finishPos =
                if Fn.eq (finish, Vector.sub (hd, finishPos)) then finishPos + 1
                else finishPos
              val len = Vector.length hd - finishPos
              val slice = VectorSlice.slice (hd, finishPos, SOME len)
              val newHd = VectorSlice.vector slice
            in
              joinStartOfRight (newHd, tl)
            end
        end
    | [] => right

  fun removeLeftFromHere (start, left) =
    case left of
      hd :: tl =>
        let
          val startPos = findInsPos (start, hd)
        in
          if startPos < 0 then
            removeLeftFromHere (start, tl)
          else if startPos = Vector.length hd then
            left
          else
            let
              (* keep first half of hd / remove last part of hd *)
              val slice = VectorSlice.slice (hd, 0, SOME startPos)
              val newHd = VectorSlice.vector slice
            in
              joinEndOfLeft (newHd, tl)
            end
        end
    | [] => left

  fun removeManyFromHd (startPos, finish, finishPos, hd, left, right) =
    let
      val lhd = VectorSlice.slice (hd, 0, SOME startPos)

      val rStart =
        if Fn.eq (finish, Vector.sub (hd, finishPos)) then finishPos + 1
        else finishPos
      val rlen = Vector.length hd - rStart
      val rhd = VectorSlice.slice (hd, rStart, SOME rlen)

      val lhd = VectorSlice.vector lhd
      val rhd = VectorSlice.vector rhd
    in
      {left = joinEndOfLeft (lhd, left), right = joinStartOfRight (rhd, right)}
    end

  fun moveLeftAndRemove (start, finish, left, right) =
    case left of
      hd :: tl =>
        let
          val finishPos = findInsPos (finish, hd)
        in
          if finishPos < 0 then
            moveLeftAndRemove (start, finish, tl, joinStartOfRight (hd, right))
          else if finishPos = Vector.length hd then
            let
              val startPos = findInsPos (start, hd)
            in
              if startPos < 0 then
                (* remove hd and continue removing leftwards *)
                let val left = removeLeftFromHere (start, left)
                in {left = left, right = right}
                end
              else if startPos = Vector.length hd then
                (* return, not removing anything, 
                 * because there are no elements
                 * between start and finish.
                 * We do still join hd to tl if pssible for performace reasons. *)
                {left = joinEndOfLeft (hd, tl), right = right}
              else
                (* have to delete from last part of hd *)
                let
                  val slice = VectorSlice.slice (hd, 0, SOME startPos)
                  val newHd = VectorSlice.vector slice
                in
                  {left = joinEndOfLeft (newHd, tl), right = right}
                end
            end
          else
            (* finish pos is somewhere in middle of hd
             * but have to check where startPos is. *)
            let
              val startPos = findInsPos (start, hd)
            in
              if startPos < 0 then
                let
                  val slice = VectorSlice.slice (hd, 0, SOME finishPos)
                  val newHd = VectorSlice.vector slice
                  val left = removeLeftFromHere (start, tl)
                in
                  {left = left, right = right}
                end
              else
                (* startPos is in middle of hd.
                 * Does not make sense for startPos = Vector.length hd
                 * because finishPos is in middle as well. 
                 * So, delete from middle. *)
                removeManyFromHd (startPos, finish, finishPos, hd, tl, right)
            end
        end
    | [] => {left = left, right = right}

  fun moveRightAndRemove (start, finish, left, right) =
    case right of
      hd :: tl =>
        let
          val startPos = findInsPos (start, hd)
        in
          if startPos = Vector.length hd then
            (* keep moving rightwards *)
            moveRightAndRemove (start, finish, joinEndOfLeft (hd, left), tl)
          else if startPos < 0 then
            (* start does not exist as it is before this node.
             * Does finish exist, and if it does, what is its position? *)
            let
              val finishPos = findInsPos (finish, hd)
            in
              if finishPos = Vector.length hd then
                (* remove this node and delete right from here. *)
                let val right = removeRightFromHere (finish, tl)
                in {left = left, right = right}
                end
              else if finishPos < 0 then
                (* finish is less than first element in this node,
                 * so return. *)
                {left = left, right = right}
              else
                (* have to delete first part of the hd *)
                let
                  val lhd = VectorSlice.slice (hd, 0, SOME startPos)

                  val rStart =
                    if Fn.eq (Vector.sub (hd, finishPos), finish) then
                      finishPos + 1
                    else
                      finishPos
                  val rLen = Vector.length hd - rStart
                  val rhd = VectorSlice.slice (hd, rStart, SOME rLen)
                  val lhd = VectorSlice.vector lhd
                  val rhd = VectorSlice.vector rhd
                in
                  { left = joinEndOfLeft (lhd, left)
                  , right = joinStartOfRight (rhd, right)
                  }
                end
            end
          else
            (* have to delete starting from this node.
             * End depends on the `finish` value. *)
            let
              val finishPos = findInsPos (finish, hd)
            in
              if finishPos = Vector.length hd then
                (* delete last part of this node 
                 * and continue deleting rightwards *)
                let
                  val hd = VectorSlice.slice (hd, 0, SOME startPos)
                  val hd = VectorSlice.vector hd
                  val tl = removeRightFromHere (finish, tl)
                in
                  {left = left, right = joinStartOfRight (hd, tl)}
                end
              else
                (* we already checked and found that 
                 * start is somewhere in the middle. 
                 * This means `finish` must be in the middle too,
                 * if finish is not equal to `Vector.length hd`. 
                 * So we only need to delete some part from the middle of hd. *)
                removeManyFromHd (startPos, finish, finishPos, hd, left, tl)
            end
        end
    | [] => {left = left, right = right}

  fun removeWhenStartIsLessThanRFirst (start, finish, left, right, rfirst) =
    case left of
      lhd :: _ =>
        let
          val llast = Vector.sub (lhd, Vector.length lhd - 1)
        in
          if Fn.l (start, llast) then
            if Fn.g (finish, llast) then
              (* have to delete left from here and right from here *)
              let
                val left = removeLeftFromHere (start, left)
                (* removeRightFromHere will not remove anything 
                 * if finish < rfirst *)
                val right = removeRightFromHere (finish, right)
              in
                {left = left, right = right}
              end
            else
              (* either finish < llast or finish = llast
               * which means move left and delete 
               * since finish may be before lhd *)
              moveLeftAndRemove (start, finish, left, right)
          else if Fn.eq (start, llast) then
            if
              Fn.eq (finish, llast)
            then
              (* just need to remove llast as both start and finish range
               * are contained in left *)
              let val left = removeLeftFromHere (start, left)
              in {left = left, right = right}
              end
            else (* finish > llast 
                  * as finish < llast case is impossible 
                  * since start = llast.
                  * Check how finish compares to rfirst. *) if
              Fn.l (finish, rfirst)
            then
              (* don't do anything with finish/rfirst, 
               * because finish is less than rfirst 
               * but do remove llast from left
               * because llast is equal to start *)
              let val left = removeLeftFromHere (start, left)
              in {left = left, right = right}
              end
            else
              (* finish >= rfirst; in either case, we need to remove 
               * some elements which are in right. *)
              let
                val left = removeLeftFromHere (start, left)
                val right = removeRightFromHere (finish, right)
              in
                {left = left, right = right}
              end
          else (* start > llast *) if Fn.l (finish, rfirst) then
            (* no elements in range between start and finish *)
            {left = left, right = right}
          else
            (* whether finish > rfirst or finish = rfirst, 
             * we have some elements to delete from the right *)
            let val right = removeRightFromHere (finish, right)
            in {left = left, right = right}
            end
        end
    | [] => {left = left, right = right}

  fun removeWhenRightIsEmpty (start, finish, left, right) =
    case left of
      hd :: tl =>
        let
          val finishPos = findInsPos (finish, hd)
          val startPos = findInsPos (start, hd)
        in
          if
            finishPos = Vector.length hd
          then
            if startPos = Vector.length hd then
              {left = left, right = right}
            else if startPos < 0 then
              (* remove hd, and continue removing leftwards *)
              let val left = removeLeftFromHere (start, left)
              in {left = left, right = right}
              end
            else
              (* remove last part of hd, keeping first part *)
              let
                val slice = VectorSlice.slice (hd, 0, SOME startPos)
                val newHd = VectorSlice.vector slice
              in
                {left = tl, right = [newHd]}
              end
          else if
            finishPos < 0
          then
            moveLeftAndRemove (start, finish, tl, [hd])
          else (* finishPos is in middle; what about startPos? *) if
            startPos < 0
          then
            moveLeftAndRemove (start, finish, left, right)
          else
            (* startPos is in middle because `start = Vector.length hd`
            * is impossible, as finish is in middle already. *)
            removeManyFromHd (startPos, finish, finishPos, hd, tl, right)
        end
    | [] => {left = left, right = right}

  (* assumption: 'start' is the minimum element to delete and 'finish' is the
   * last element to delete. 
   * Reason for this assumption is because we don't ask the user for a function
   * like `Int.min` or `Int.max` which can be used to get the minimum/maximum.
   * So, if the user passes in a `start` that is greater than a `finish`,
   * then that's a user error. *)
  fun removeMany (start, finish, {left, right}) =
    case right of
      rhd :: _ =>
        let
          val rfirst = Vector.sub (rhd, 0)
        in
          if Fn.g (start, rfirst) then
            (* Will need to move rightwards and delete. *)
            moveRightAndRemove (start, finish, left, right)
          else if Fn.eq (start, rfirst) then
            (* need to delete right from here *)
            let val right = removeRightFromHere (finish, right)
            in {left = left, right = right}
            end
          else
            removeWhenStartIsLessThanRFirst (start, finish, left, right, rfirst)
        end
    | [] => removeWhenRightIsEmpty (start, finish, left, right)

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
