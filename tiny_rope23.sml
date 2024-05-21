structure TinyRope23 =
struct
  (* Type of ropes. *)
  datatype t =
    Leaf of string
  | N2 of t * int * t * int
  | N3 of t * int * t * int * t * int

  fun foldl f state rope =
    case rope of
      Leaf str => f (str, state)
    | N2 (l, _, r, _) => let val state = foldl f state l in foldl f state r end
    | N3 (l, _, m, _, r, _) =>
        let
          val state = foldl f state l
          val state = foldl f state m
        in
          foldl f state r
        end

  fun foldr f state rope =
    case rope of
      Leaf str => f (str, state)
    | N2 (l, _, r, _) => let val state = foldr f state r in foldr f state l end
    | N3 (l, _, m, _, r, _) =>
        let
          val state = foldr f state r
          val state = foldr f state m
        in
          foldr f state l
        end

  local
    fun toListFolder (str, lst) = str :: lst
    fun toList rope =
      foldr toListFolder [] rope
  in
    fun toString rope =
      let val lst = toList rope
      in String.concat lst
      end
  end

  (* Type used for balancing ropes, used only internally. *)
  datatype treeI =
    TI of t * int
  | OF of t * int * t * int

  val targetLength = 1024
  val empty = Leaf ""
  fun fromString string = Leaf string

  fun size rope =
    case rope of
      Leaf str => String.size str
    | N2 (_, lm, _, rm) => rm + rm
    | N3 (_, lm, _, mm, _, rm) => lm + mm + rm

  fun isLessThanTarget (str1, str2) =
    String.size str1 + String.size str2 <= targetLength

  fun insLeaf (curIdx, newStr, oldStr) =
    if curIdx <= 0 then
      if isLessThanTarget (oldStr, newStr) then
        let val str = newStr ^ oldStr
        in TI (Leaf str, String.size str)
        end
      else
        OF (Leaf newStr, String.size newStr, Leaf oldStr, String.size oldStr)
    else if curIdx >= String.size oldStr then
      if isLessThanTarget (oldStr, newStr) then
        let val str = oldStr ^ newStr
        in TI (Leaf str, String.size str)
        end
      else
        OF (Leaf oldStr, String.size oldStr, Leaf newStr, String.size newStr)
    else
      (* Need to split in middle of string. *)
      let
        val sub1 = String.substring (oldStr, 0, curIdx)
        val sub2Len = String.size oldStr - curIdx
        val sub2 = String.substring (oldStr, curIdx, sub2Len)
      in
        if
          isLessThanTarget (oldStr, newStr)
        then
          let val str = sub1 ^ newStr ^ sub2
          in TI (Leaf str, String.size str)
          end
        else if
          curIdx + String.size newStr <= targetLength
        then
          let
            val leftString = sub1 ^ newStr
          in
            OF
              ( Leaf leftString
              , String.size leftString
              , Leaf sub2
              , String.size sub2
              )
          end
        else if
          ((String.size oldStr) - curIdx) + String.size newStr <= targetLength
        then
          let
            val rightString = newStr ^ sub2
          in
            OF
              ( Leaf sub1
              , String.size sub1
              , Leaf rightString
              , String.size rightString
              )
          end
        else
          let
            val left =
              N2 (Leaf sub1, String.size sub1, Leaf newStr, String.size newStr)
            val leftSize = String.size sub1 + String.size newStr
            val right = N2 (Leaf sub2, String.size sub2, empty, 0)
            val rightSize = String.size sub2
          in
            OF (left, leftSize, right, rightSize)
          end
      end

  fun ins (curIdx, newStr, rope) =
    case rope of
      N2 (l, lm, r, rm) =>
        if curIdx < lm then
          (case ins (curIdx, newStr, l) of
             TI (l, lm) => TI (N2 (l, lm, r, rm), lm + rm)
           | OF (l1, lm1, l2, lm2) =>
               TI (N3 (l1, lm1, l2, lm2, r, rm), lm1 + lm2 + rm))
        else
          (case (ins (curIdx - lm, newStr, r)) of
             TI (r, rm) => TI (N2 (l, lm, r, rm), lm + rm)
           | OF (r1, rm1, r2, rm2) =>
               TI (N3 (l, lm, r1, rm1, r2, rm2), lm + rm1 + rm2))
    | N3 (l, lm, m, mm, r, rm) =>
        (*
         * Ropes don't usually have N3 nodes so the way we accomodate this is:
         * If current index is less than left metadata, use same strategy as
         * recursing to the left as N2 nodes.
         * Else if current index is less than (left + middle) metadata, 
         * recurse to middle node while subtracting left metadata.
         * Else, recurse to right node while subtracting (left metadata +
         * middle metadata).
         * This simulates the mathematical operations that would take place
         * for the following rope:
         *       (l, lm)
         *     /         \
         * (..., ...)   (m, mm, r, rm)
         *)
        if curIdx < lm then
          (case ins (curIdx, newStr, l) of
             TI (l, lm) => TI (N3 (l, lm, m, mm, r, rm), lm + mm + rm)
           | OF (l1, lm1, l2, lm2) =>
               OF (N2 (l1, lm1, l2, lm2), lm1 + lm2, N2 (m, mm, r, rm), mm + rm))
        else if curIdx < (lm + mm) then
          (case ins (curIdx - lm, newStr, m) of
             TI (m, mm) => TI (N3 (l, lm, m, mm, r, rm), lm + mm + rm)
           | OF (m1, mm1, m2, mm2) =>
               OF (N2 (l, lm, m1, mm1), lm + mm1, N2 (m2, mm2, r, rm), mm2 + rm))
        else
          (case ins (curIdx - (lm + mm), newStr, r) of
             TI (r, rm) => TI (N3 (l, lm, m, mm, r, rm), lm + mm + rm)
           | OF (r1, rm1, r2, rm2) =>
               OF (N2 (l, lm, m, mm), lm + mm, N2 (r1, rm1, r2, rm2), rm1 + rm2))
    | Leaf oldStr => insLeaf (curIdx, newStr, oldStr)

  fun insRoot (TI (t, _)) = t
    | insRoot (OF (l, lm, r, rm)) = N2 (l, lm, r, rm)

  fun insert (idx, newStr, rope) =
    insRoot (ins (idx, newStr, rope))

  datatype treeD = TD of t | UF of t

  exception RopeDeleteError

  fun node21 (TD t1, t2) =
        let val tree = N2 (t1, size t1, t2, size t2)
        in TD (tree)
        end
    | node21 (UF t1, N2 (t2, t2m, t3, t3m)) =
        let val tree = N3 (t1, size t1, t2, t2m, t3, t3m)
        in UF (tree)
        end
    | node21 (UF t1, N3 (t2, t2m, t3, t3m, t4, t4m)) =
        let
          val t1m = size t1
          val left = N2 (t1, t1m, t2, t2m)
          val right = N2 (t3, t3m, t4, t4m)
          val tree = N2 (left, t1m + t2m, right, t3m + t4m)
        in
          TD (tree)
        end
    | node21 _ = raise RopeDeleteError

  fun node22 (t1, t1m, TD t2) =
        TD (N2 (t1, t1m, t2, size t2))
    | node22 (N2 (t1, t1m, t2, t2m), _, UF t3) =
        UF (N3 (t1, t1m, t2, t2m, t3, size t3))
    | node22 (N3 (t1, t1m, t2, t2m, t3, t3m), _, UF t4) =
        let
          val t4m = size t4
        in
          TD (N2
            (N2 (t1, t1m, t2, t2m), t1m + t2m, N2 (t3, t3m, t4, t4m), t3m + t4m))
        end
    | node22 _ = raise RopeDeleteError

  fun node31 (TD t1, t2, t2m, t3, t3m) =
        TD (N3 (t1, size t1, t2, t2m, t3, t3m))
    | node31 (UF t1, N2 (t2, t2m, t3, t3m), _, t4, t4m) =
        let
          val t1m = size t1
          val left = N3 (t1, t1m, t2, t2m, t3, t3m)
          val leftSize = t1m + t2m + t3m
          val inner = N2 (left, leftSize, t4, t4m)
        in
          TD inner
        end
    | node31 (UF t1, N3 (t2, t2m, t3, t3m, t4, t4m), _, t5, t5m) =
        let
          val t1m = size t1
          val left = N2 (t1, t1m, t2, t2m)
          val leftSize = t1m + t2m

          val middle = N2 (t3, t3m, t4, t4m)
          val middleSize = t3m + t4m

          val inner = N3 (left, leftSize, middle, middleSize, t5, t5m)
        in
          TD inner
        end
    | node31 _ = raise RopeDeleteError

  fun node32 (t1, t1m, TD t2, t3) =
        TD (N3 (t1, t1m, t2, size t2, t3, size t3))
    | node32 (t1, t1m, UF t2, N2 (t3, t3m, t4, t4m)) =
        let
          val t2m = size t2
          val right = N3 (t2, t2m, t3, t3m, t4, t4m)
          val inner = N2 (t1, t1m, right, t2m + t3m + t4m)
        in
          TD inner
        end
    | node32 (t1, t1m, UF t2, N3 (t3, t3m, t4, t4m, t5, t5m)) =
        let
          val t2m = size t2
          val mid = N2 (t2, t2m, t3, t3m)
          val right = N2 (t4, t4m, t5, t5m)
          val inner = N3 (t1, t1m, mid, t2m + t3m, right, t4m + t5m)
        in
          TD inner
        end
    | node32 _ = raise RopeDeleteError

  fun node33 (t1, t1m, t2, t2m, TD t3) =
        TD (N3 (t1, t1m, t2, t2m, t3, size t3))
    | node33 (t1, t1m, N2 (t2, t2m, t3, t3m), _, UF t4) =
        let val t4m = size t4
        in TD (N2 (t1, t1m, N3 (t2, t2m, t3, t3m, t4, t4m), t2m + t3m + t4m))
        end
    | node33 (t1, t1m, N3 (t2, t2m, t3, t3m, t4, t4m), _, UF t5) =
        let
          val t5m = size t4
        in
          TD (N3
            ( t1
            , t1m
            , N2 (t2, t2m, t3, t3m)
            , t2m + t3m
            , N2 (t4, t4m, t5, t5m)
            , t4m + t5m
            ))
        end
    | node33 _ = raise RopeDeleteError

end
