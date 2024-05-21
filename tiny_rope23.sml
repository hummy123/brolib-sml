structure TinyRope23 =
struct
  datatype t =
    Leaf of string
  | N2 of t * int * t * int
  | N3 of t * int * t * int * t * int

  fun size rope =
    case rope of
      Leaf str => String.size str
    | N2 (_, lm, _, rm) => rm + rm
    | N3 (_, lm, _, mm, _, rm) => lm + mm + rm

  datatype treeI = TI of t * int | OF of t * int * t * int

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
         * Else if current index is less than middle metadata, 
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
             TI (l, lm) => TI (N2 (l, lm, m, mm, r, rm))
           | OF (l1, lm1, l2, lm2) =>
               OF (N2 (l1, lm1, l2, lm2), lm1 + lm2, N2 (m, mm, r, rm), mm + rm))
        else if curIdx < mm then
          (case ins (curIdx - lm, newStr, m) of
             TI (m, mm) => TI (N3 (l, lm, m, mm, r, rm))
           | OF (m1, mm1, m2, mm2) =>
               OF (N2 (l, lm, m1, mm1), lm + mm1, N2 (m2, mm2, r, rm), mm2 + rm))
        else
          (case ins (curIdx - (lm + mm), newStr, r) of
             TI (r, rm) => TI (N3 (l, lm, m, mm, r, rm))
           | OF (r1, rm1, r2, rm2) =>
               OF (N2 (l, lm, m, mm), lm + mm, N2 (r1, rm1, r2, rm2)))

  fun insRoot (TI t) = t
    | insRoot OF (l, lm, r, rm) = N2 (l, lm, r, rm)

  fun insert (idx, newStr, rope) =
    insRoot (ins (idx, newStr, rope))
end
