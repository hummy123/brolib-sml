signature ROPE =
sig
  type t
  val empty: t
  val fromString: string -> t
  val foldr: ('a * string * int vector -> 'a) * 'a * t -> 'a
end

structure Rope :> ROPE =
struct
  (* This function counts line breaks in reverse order, 
   * from the end of the string to the start.
   * Reverse order is used for performance, as it avoids a List.rev at the end. *)
  fun helpCountLineBreaks (pos, acc, str) =
    if pos < 0 then
      Vector.fromList acc
    else
      let
        val chr = String.sub (str, pos)
      in
        if chr = #"\n" then
          (* Is this a \r\n pair? Then the position of \r should be consed. *)
          if pos = 0 then
            Vector.fromList (0 :: acc)
          else
            let
              val prevChar = String.sub (str, pos - 1)
            in
              if prevChar = #"\r" then
                helpCountLineBreaks (pos - 2, (pos - 1) :: acc, str)
              else
                helpCountLineBreaks (pos - 2, pos :: acc, str)
            end
        else if chr = #"\r" then
          helpCountLineBreaks (pos - 1, pos :: acc, str)
        else
          helpCountLineBreaks (pos - 1, acc, str)
      end

  fun countLineBreaks str =
    helpCountLineBreaks (String.size str - 1, [], str)

  (* Binary search. Used to find split point in vector. *)
  fun binSearch (findNum, lines, low, high) =
    if Vector.length lines = 0 then
      0
    else
      let
        val mid = low + ((high - low) div 2)
      in
        if high >= low then
          let
            val midVal = Vector.sub (lines, mid)
          in
            if midVal = findNum then
              mid
            else if midVal < findNum then
              binSearch (findNum, lines, mid + 1, high)
            else
              binSearch (findNum, lines, low, mid - 1)
          end
        else
          mid
      end

  datatype t =
    N0 of string * int vector
  | N1 of t
  | N2 of t * int * int * t
  | L2 of string * int vector * string * int vector
  | N3 of t * t * t

  exception AuxConstructor

  exception Substring of int

  fun foldrString (f, state, rope) =
    case rope of
      N2 (l, _, _, r) =>
        let val state = foldrString (f, state, r)
        in foldrString (f, state, l)
        end
    | N1 t => foldrString (f, state, t)
    | N0 (s, _) => f (state, s)
    | _ => raise AuxConstructor

  fun toString rope =
    let val strList = foldrString ((fn (acc, str) => str :: acc), [], rope)
    in String.concat strList
    end

  fun foldr (f, state, rope) =
    case rope of
      N2 (l, _, _, r) =>
        let val state = foldr (f, state, r)
        in foldr (f, state, l)
        end
    | N1 t => foldr (f, state, t)
    | N0 (s, v) => f (state, s, v)
    | _ => raise AuxConstructor

  datatype balance = AddedNode | DeletedNode | NoAction

  val targetLength = 1024
  val targetVecLength = 128

  fun id x = x
  val emptyVec = Vector.tabulate (0, id)
  val empty = N0 ("", emptyVec)

  fun fromString string =
    let val lineBreaks = countLineBreaks string
    in N0 (string, lineBreaks)
    end

  fun isLessThanTarget (str1, str2, vec1, vec2) =
    String.size str1 + String.size str2 <= targetLength
    andalso Vector.length vec1 + Vector.length vec2 <= targetVecLength

  (* This function creates a new node in the rope, calculating right-metadata.
   * This is equivalent to helpSize/size in tiny_rope.ml, 
   * but because the size function in tiny_rope.ml was only used 
   * by callers to construct N2 cases, 
   * it can be replaced with a function that constructs N2 cases
   * instead of returning (int * int) metadata which results in
   * an extra tuple allocation. *)
  fun helpMakeN2 (idxAcc, lineAcc, left, right, rope) =
    case rope of
      N2 (_, lms, lmv, r) =>
        helpMakeN2 (lms + idxAcc, lmv + lineAcc, left, right, r)
    | N1 t => helpMakeN2 (idxAcc, lineAcc, left, right, t)
    | N0 (str, lines) =>
        let
          val idxAcc = idxAcc + String.size str
          val lineAcc = lineAcc + Vector.length lines
        in
          N2 (left, idxAcc, lineAcc, right)
        end
    | _ => raise AuxConstructor

  (* Accumulate right-metadata for left rope. *)
  fun makeN2 (left, right) =
    helpMakeN2 (0, 0, left, right, left)

  fun insL2 (s1, v1, s2, v2) =
    let
      val left = N0 (s1, v1)
      val right = N0 (s2, v2)
    in
      N2 (left, String.size s1, Vector.length v1, right)
    end

  fun insN3 (t1, t2, t3) =
    let
      val left = makeN2 (t1, t2)
      val right = N1 t3
    in
      makeN2 (left, right)
    end

  fun insRoot rope =
    case rope of
      L2 (s1, v1, s2, v2) => insL2 (s1, v1, s2, v2)
    | N3 (t1, t2, t3) => insN3 (t1, t2, t3)
    | t => t

  fun delRoot rope =
    case rope of
      N1 t => t
    | t => t

  fun insN1 rope =
    case rope of
      L2 (s1, v1, s2, v2) => insL2 (s1, v1, s2, v2)
    | N3 (t1, t2, t3) => insN3 (t1, t2, t3)
    | t => N1 t

  fun insN2Left (left, right) =
    case (left, right) of
      (L2 (s1, v1, s2, v2), t3) =>
        let
          val left = N0 (s1, v1)
          val middle = N0 (s2, v2)
        in
          N3 (left, middle, t3)
        end
    | (N3 (t1, t2, t3), N1 t4) =>
        let
          val left = makeN2 (t1, t2)
          val right = makeN2 (t3, t4)
        in
          makeN2 (left, right)
        end
    | (N3 (t1, t2, t3), t4) =>
        let
          val left = makeN2 (t1, t2)
          val middle = N1 t3
        in
          N3 (left, middle, t4)
        end
    | (l, r) => makeN2 (l, r)

  fun delN2Left (left, right) =
    case (left, right) of
      (N1 t1, N1 t2) => let val inner = makeN2 (t1, t2) in N1 inner end
    | (N1 (N1 t1), N2 (N1 t2, _, _, (t3 as N2 _))) =>
        let
          val left = makeN2 (t1, t2)
          val inner = makeN2 (left, t3)
        in
          N1 inner
        end
    | (N1 (N1 t1), N2 (N2 (t2, _, _, t3), _, _, N1 t4)) =>
        let
          val left = makeN2 (t1, t2)
          val right = makeN2 (t3, t4)
          val inner = makeN2 (left, right)
        in
          N1 inner
        end
    | (N1 (t1 as N1 _), N2 ((t2 as N2 _), _, _, (t3 as N2 _))) =>
        let
          val left = makeN2 (t1, t2)
          val right = N1 t3
        in
          makeN2 (left, right)
        end
    | (l, r) => makeN2 (l, r)

  fun insN2Right (left, right) =
    case (left, right) of
      (t1, L2 (s1, v1, s2, v2)) =>
        let
          val middle = N0 (s1, v1)
          val right = N0 (s2, v2)
        in
          N3 (t1, middle, right)
        end
    | (N1 t1, N3 (t2, t3, t4)) =>
        let
          val left = makeN2 (t1, t2)
          val right = makeN2 (t3, t4)
        in
          makeN2 (left, right)
        end
    | (t1, N3 (t2, t3, t4)) =>
        let
          val right = makeN2 (t3, t4)
          val middle = N1 t2
        in
          N3 (t1, middle, right)
        end
    | (l, r) => makeN2 (l, r)

  fun delN2Right (left, right) =
    case (left, right) of
      (N2 (N1 t1, _, _, N2 (t2, _, _, t3)), N1 (N1 t4)) =>
        let
          val left = makeN2 (t1, t2)
          val right = makeN2 (t3, t4)
          val inner = makeN2 (left, right)
        in
          N1 inner
        end
    | (N2 ((t1 as N2 _), lms, lmv, N1 t2), N1 (N1 t3)) =>
        let
          val right = makeN2 (t2, t3)
          val inner = N2 (t1, lms, lmv, right)
        in
          N1 inner
        end
    | (N2 ((t1 as N2 _), _, _, (t2 as N2 _)), N1 (t3 as N1 _)) =>
        let
          val left = N1 t1
          val right = makeN2 (t2, t3)
        in
          makeN2 (left, right)
        end
    | (l, r) => makeN2 (l, r)

  fun insVecBefore (oldVec, newVec, newStr) =
    let
      val oldLen = Vector.length oldVec
      val newLen = Vector.length newVec
      val total = oldLen + newLen
      val newStrLen = String.size newStr
    in
      Vector.tabulate (total, (fn (idx) =>
        if idx < newLen then Vector.sub (newVec, idx)
        else Vector.sub (oldVec, idx - newLen) + newStrLen))
    end

  fun insVecAfter (oldStr, oldVec, newVec) =
    let
      val oldLen = Vector.length oldVec
      val newLen = Vector.length newVec
      val total = oldLen + newLen
      val oldStrLen = String.size oldStr
    in
      Vector.tabulate (total, (fn (idx) =>
        if idx < oldLen then Vector.sub (oldVec, idx)
        else Vector.sub (newVec, idx - oldLen) + oldStrLen))
    end

  fun insLeaf (curIdx, newStr, newVec, rope, oldStr, oldVec) =
    if curIdx <= 0 then
      if isLessThanTarget (oldStr, newStr, oldVec, newVec) then
        let
          val str = newStr ^ oldStr
          val vec = insVecBefore (oldVec, newVec, newStr)
        in
          (N0 (str, vec), NoAction)
        end
      else
        let val l2 = L2 (newStr, newVec, oldStr, oldVec)
        in (l2, AddedNode)
        end
    else if curIdx >= String.size oldStr then
      if isLessThanTarget (oldStr, newStr, oldVec, newVec) then
        let
          val str = oldStr ^ newStr
          val vec = insVecAfter (oldStr, oldVec, newVec)
        in
          (N0 (str, vec), NoAction)
        end
      else
        let val l2 = L2 (oldStr, oldVec, newStr, newVec)
        in (l2, AddedNode)
        end
    else
      (* Need to split in middle of string. *)
      let
        val sub1 = String.substring (oldStr, 0, curIdx)
        val sub2Len = String.size oldStr - curIdx
        val sub2 = String.substring (oldStr, curIdx, sub2Len)

        val oldVecLen = Vector.length oldVec
        val midPoint = binSearch (String.size sub1, oldVec, 0, oldVecLen)

        val newVecLen = Vector.length newVec
      in
        if
          isLessThanTarget (oldStr, newStr, oldVec, newVec)
        then
          let
            val str = sub1 ^ newStr ^ sub2
            val totalVecLen = Vector.length oldVec + Vector.length newVec
            val vec = Vector.tabulate (totalVecLen, (fn idx =>
              if idx < midPoint then
                Vector.sub (oldVec, idx)
              else if idx < midPoint + newVecLen then
                Vector.sub (newVec, idx - midPoint)
              else
                Vector.sub (oldVec, idx - newVecLen)))
          in
            (N0 (str, vec), NoAction)
          end
        else if
          curIdx + String.size newStr <= targetLength
          andalso midPoint + newVecLen <= targetVecLength
        then
          let
            val str1 = sub1 ^ newStr
            val vec1 = Vector.tabulate (midPoint + newVecLen, (fn idx =>
              if idx < midPoint then Vector.sub (oldVec, idx)
              else Vector.sub (newVec, idx - midPoint)))

            val vec2 = Vector.tabulate (oldVecLen - midPoint, (fn idx =>
              Vector.sub (oldVec, idx + midPoint)))

            val l2 = L2 (str1, vec1, sub2, vec2)
          in
            (l2, AddedNode)
          end
        else if
          ((String.size oldStr) - curIdx) + String.size newStr <= targetLength
          andalso (midPoint - oldVecLen) + newVecLen <= targetVecLength
        then
          let
            val str2 = newStr ^ sub2
            val newStrLen = String.size newStr
            val vec2 =
              Vector.tabulate ((midPoint - oldVecLen) + newVecLen, (fn idx =>
                if idx < newVecLen then Vector.sub (newVec, idx)
                else Vector.sub (oldVec, idx - newVecLen) + newStrLen))

            val vec1 = Vector.tabulate (midPoint, (fn idx =>
              Vector.sub (oldVec, idx)))

            val l2 = L2 (sub1, vec1, str2, vec2)
          in
            (l2, AddedNode)
          end
        else
          let
            val vec1 =
              if oldVecLen = 0 then
                emptyVec
              else
                Vector.tabulate (midPoint, (fn idx => Vector.sub (oldVec, idx)))

            val vec2 =
              if oldVecLen = 0 orelse midPoint >= oldVecLen then
                emptyVec
              else
                Vector.tabulate (oldVecLen - midPoint, (fn idx =>
                  Vector.sub (oldVec, midPoint + idx)))

            val left = N0 (sub1, vec1)
            val right = N0 (sub2, vec2)
            val mid = N0 (newStr, newVec)
          in
            (N3 (left, right, mid), AddedNode)
          end
      end

end