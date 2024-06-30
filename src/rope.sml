signature ROPE =
sig
  type t
  val empty: t

  val fromString: string -> t
  val toString: t -> string

  (* The caller should not insert in the middle of a \r\n pair,
   * or else line metadata will become invalid. *)
  val insert: int * string * t -> t

  (* The append and appendLine function both add a string to the end.
   * The difference is that append calculates line metadata 
   * from the given string, while appendLine accepts 
   * (possibly incorrect) metadata from the caller. *)
  val append: string * t -> t
  val appendLine: string * int vector * t -> t

  (* The caller should not delete only a single character in a \r\n pair,
   * because then line metadata will become invalid. *)
  val delete: int * int * t -> t

  (* Folds over the characters in the rope starting from the index 
   * in the second parameter. *)
  val foldFromIdx: (char * 'a -> 'a) * int * t * 'a -> 'a

  (* Like the foldFromIdx function, but accepts a predicate as the second
   * argument. 
   * If the predicate returns true, terminates and returns the result;
   * else, continues folding until predicate returns true or until remaining
   * characters have been traversed. *)
  val foldFromIdxTerm: (char * 'a -> 'a) * ('a -> bool) * int * t * 'a -> 'a

  (* This function folds over the characters in the rope, 
   * starting from a given line number. 
   * The second argument is a predicate indicating when to stop folding. *)
  val foldLines: (char * 'a -> 'a) * ('a -> bool) * int * t * 'a -> 'a

  (* This below function is just for testing. 
   * It verifies that line metadata is as expected,
   * raising an exception if it is different, 
   * and returning true if it is the same. *)
  val verifyLines: t -> bool
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
                helpCountLineBreaks (pos - 1, pos :: acc, str)
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
      Vector.tabulate (total, (fn idx =>
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
      Vector.tabulate (total, (fn idx =>
        if idx < oldLen then Vector.sub (oldVec, idx)
        else Vector.sub (newVec, idx - oldLen) + oldStrLen))
    end

  fun preLeaf (oldStr, oldVec, newStr, newVec) =
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

  fun appLeaf (oldStr, oldVec, newStr, newVec) =
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

  fun insLeaf (curIdx, newStr, newVec, oldStr, oldVec) =
    if curIdx <= 0 then
      preLeaf (oldStr, oldVec, newStr, newVec)
    else if curIdx >= String.size oldStr then
      appLeaf (oldStr, oldVec, newStr, newVec)
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

  fun insLMoreThanTarget (lms, newStr, lmv, newVec, l, r, action) =
    let
      val lms = lms + String.size newStr
      val lmv = lmv + Vector.length newVec
      val node = N2 (l, lms, lmv, r)
    in
      (node, action)
    end

  fun insLessThanTarget (s1, s2, v1, v2) =
    let
      val str = s1 ^ s2
      val s1Len = String.size s1
      val v1Len = Vector.length v1
      val v2Len = Vector.length v2
      val vec = Vector.tabulate (v1Len + v2Len, (fn idx =>
        if idx < v1Len then Vector.sub (v1, idx)
        else Vector.sub (v2, idx - v1Len) + s1Len))
      val node = N0 (str, vec)
    in
      (node, DeletedNode)
    end

  fun insBalL (l, lms, lmv, newStr, newVec, r, action) =
    (case action of
       NoAction =>
         (case (l, r) of
            (N0 (s1, v1), N0 (s2, v2)) =>
              if isLessThanTarget (s1, s2, v1, v2) then
                insLessThanTarget (s1, s2, v1, v2)
              else
                insLMoreThanTarget (lms, newStr, lmv, newVec, l, r, action)
          | _ => insLMoreThanTarget (lms, newStr, lmv, newVec, l, r, action))
     | AddedNode => (insN2Left (l, r), action)
     | DeletedNode => (delN2Left (l, r), action))

  fun insBalR (l, r, action) =
    (case action of
       NoAction =>
         (case (l, r) of
            (N0 (s1, v1), N0 (s2, v2)) =>
              if isLessThanTarget (s1, s2, v1, v2) then
                insLessThanTarget (s1, s2, v1, v2)
              else
                (makeN2 (l, r), action)
          | _ => (makeN2 (l, r), action))
     | AddedNode => (insN2Right (l, r), action)
     | DeletedNode => (delN2Right (l, r), action))

  fun ins (curIdx, newStr, newVec, rope) =
    case rope of
      N2 (l, lms, lmv, r) =>
        if curIdx < lms then
          let val (l, action) = ins (curIdx, newStr, newVec, l)
          in insBalL (l, lms, lmv, newStr, newVec, r, action)
          end
        else
          let val (r, action) = ins (curIdx - lms, newStr, newVec, r)
          in insBalR (l, r, action)
          end
    | N1 t =>
        let
          val (t, action) = ins (curIdx, newStr, newVec, t)
        in
          (case action of
             AddedNode => (insN1 t, action)
           | _ => (N1 t, action))
        end
    | N0 (oldStr, oldVec) => insLeaf (curIdx, newStr, newVec, oldStr, oldVec)
    | _ => raise AuxConstructor

  fun endInsert (rope, action) =
    case action of
      NoAction => rope
    | AddedNode => insRoot rope
    | DeletedNode => delRoot rope

  fun insert (index, str, rope) =
    let
      val newVec = countLineBreaks str
      val (rope, action) = ins (index, str, newVec, rope)
    in
      endInsert (rope, action)
    end

  fun app (newStr, newVec, rope) =
    case rope of
      N2 (l, lms, lmv, r) =>
        let val (r, action) = app (newStr, newVec, r)
        in insBalR (l, r, action)
        end
    | N1 t => app (newStr, newVec, t)
    | N0 (oldStr, oldVec) => appLeaf (oldStr, oldVec, newStr, newVec)
    | _ => raise AuxConstructor

  fun append (newStr, rope) =
    let
      val newVec = countLineBreaks newStr
      val (rope, action) = app (newStr, newVec, rope)
    in
      endInsert (rope, action)
    end

  fun appendLine (newStr, newVec, rope) =
    let val (rope, action) = app (newStr, newVec, rope)
    in endInsert (rope, action)
    end

  fun isDelLessThanTarget (str1, str2, vec, startPoint, endPoint) =
    let
      val vecLength = Vector.length vec - (endPoint - startPoint)
    in
      String.size str1 + String.size str2 <= targetLength
      andalso vecLength <= targetVecLength
    end


  fun delLeaf (startIdx, endIdx, str, vec) =
    if
      startIdx <= 0 andalso endIdx >= String.size str
    then
      (empty, false)
    else if
      startIdx > 0 andalso endIdx < String.size str
    then
      let
        val sub1 = String.substring (str, 0, startIdx)
        val sub2 = String.substring (str, endIdx, (String.size str - endIdx))

        val vecLength = Vector.length vec - 1
        val startPoint = binSearch (startIdx, vec, 0, vecLength)
        val endPoint = binSearch (endIdx, vec, 0, vecLength)
        val difference = endIdx - startIdx
      in
        if isDelLessThanTarget (sub1, sub2, vec, startPoint, endPoint) then
          let
            val str = sub1 ^ sub2
            val vecDifference = endPoint - startPoint
            val vecLength = Vector.length vec - vecDifference
            val vec = Vector.tabulate (vecLength, (fn idx =>
              let val point = Vector.sub (vec, idx)
              in if point < startIdx then point else point - difference
              end))
          in
            (N0 (str, vec), false)
          end
        else
          let
            val vec1 =
              if Vector.length vec = 0 then
                emptyVec
              else
                Vector.tabulate (startPoint, (fn idx => Vector.sub (vec, idx)))

            val vec2 =
              if Vector.length vec = 0 then
                emptyVec
              else
                Vector.tabulate (Vector.length vec - startPoint, (fn idx =>
                  Vector.sub (vec, idx + startPoint) - difference))
          in
            (L2 (sub1, vec1, sub2, vec2), true)
          end
      end
    else if
      startIdx >= 0 andalso startIdx <= String.size str
      andalso endIdx >= String.size str
    then
      let
        val str = String.substring (str, 0, startIdx)
        val midPoint = binSearch (startIdx, vec, 0, Vector.length vec - 1)
        val vec =
          if Vector.length vec = 0 then emptyVec
          else Vector.tabulate (midPoint, fn idx => Vector.sub (vec, idx))
      in
        (N0 (str, vec), false)
      end
    else
      let
        val str = String.substring (str, endIdx, String.size str - endIdx)
        val midPoint = binSearch (endIdx, vec, 0, Vector.length vec - 1)
        val vec =
          if Vector.length vec = 0 then
            emptyVec
          else
            Vector.tabulate (Vector.length vec - midPoint, fn idx =>
              Vector.sub (vec, idx + midPoint))
      in
        (N0 (str, vec), false)
      end

  fun del (startIdx, endIdx, rope) =
    case rope of
      N2 (l, lms, lmv, r) =>
        if lms > startIdx andalso lms > endIdx then
          let
            val (l, didIns) = del (startIdx, endIdx, l)
            val rope = if didIns then insN2Left (l, r) else makeN2 (l, r)
          in
            (rope, didIns)
          end
        else if lms < startIdx andalso lms < endIdx then
          let
            val (r, didIns) = del (startIdx - lms, endIdx - lms, r)
            val rope = if didIns then insN2Right (l, r) else makeN2 (l, r)
          in
            (rope, didIns)
          end
        else
          let
            val (l, _) = del (startIdx, endIdx, l)
            val (r, _) = del (startIdx - lms, endIdx - lms, r)
          in
            (makeN2 (l, r), false)
          end
    | N1 t => del (startIdx, endIdx, t)
    | N0 (str, vec) => delLeaf (startIdx, endIdx, str, vec)
    | _ => raise AuxConstructor

  fun delete (start, length, rope) =
    let val (rope, didIns) = del (start, start + length, rope)
    in if didIns then insRoot rope else rope
    end

  fun foldStringChars (apply, term, pos, str, strSize, acc) =
    if pos < strSize then
      if term acc then
        acc
      else
        let
          val chr = String.sub (str, pos)
          val acc = apply (chr, acc)
        in
          foldStringChars (apply, term, pos + 1, str, strSize, acc)
        end
    else
      acc

  fun foldFromIdxTerm (apply, term, idx, rope, acc) =
    case rope of
      N2 (l, lm, _, r) =>
        if idx < lm then
          let
            val acc = foldFromIdxTerm (apply, term, idx, l, acc)
          in
            if term acc then acc
            else foldFromIdxTerm (apply, term, idx - lm, r, acc)
          end
        else
          foldFromIdxTerm (apply, term, idx - lm, r, acc)
    | N1 t => foldFromIdxTerm (apply, term, idx, t, acc)
    | N0 (str, _) =>
        foldStringChars (apply, term, idx, str, String.size str, acc)
    | _ => raise AuxConstructor

  fun noTerm _ = false

  fun foldFromIdx (apply, idx, rope, acc) =
    foldFromIdxTerm (apply, noTerm, idx, rope, acc)

  fun foldLineCharsTerm (apply, term, pos, str, strSize, acc) =
    if pos < strSize then
      case term acc of
        false =>
          let
            val chr = String.sub (str, pos)
            val acc = apply (chr, acc)
          in
            foldLineCharsTerm (apply, term, pos + 1, str, strSize, acc)
          end
      | true => acc
    else
      acc

  fun helpFoldLines (apply, term, lineNum, rope, acc) =
    case rope of
      N2 (l, _, lmv, r) =>
        if lineNum < lmv then
          let
            val acc = helpFoldLines (apply, term, lineNum, rope, acc)
          in
            if term acc then acc
            else helpFoldLines (apply, term, lineNum - lmv, r, acc)
          end
        else
          helpFoldLines (apply, term, lineNum - lmv, r, acc)
    | N1 t => helpFoldLines (apply, term, lineNum, t, acc)
    | N0 (str, vec) =>
        (* We have a few edge cases to handle here. 
         * 1. If lineNum is 0 or the vector has no elements, 
         * we should start folding from the start of the string.
         * 2. Since the vector points to the start of a linebreak 
         * (which means either \r or \n when either is alone, 
         * or \r in a \r\n pair),
         * we have to skip the linebreak or linebreak pair when folding
         * over the string. That is more intuitive to the user. *)
        if lineNum < 0 orelse Vector.length vec = 0 then
          foldLineCharsTerm (apply, term, 0, str, String.size str, acc)
        else
          let
            val idx = Vector.sub (vec, lineNum)
          in
            if idx + 1 < String.size str then
              let
                val chr = String.sub (str, idx)
                val nextChr = String.sub (str, idx + 1)
              in
                if chr = #"\r" andalso nextChr = #"\n" then
                  foldLineCharsTerm
                    (apply, term, idx + 2, str, String.size str, acc)
                else
                  foldLineCharsTerm
                    (apply, term, idx + 1, str, String.size str, acc)
              end
            else
              acc
          end
    | _ => raise AuxConstructor

  fun foldLines (apply, term, lineNum, rope, acc) =
    helpFoldLines (apply, term, lineNum - 1, rope, acc)

  fun verifyLines rope =
    foldr
      ( (fn (_, str, vec) =>
           let
             val strVec = countLineBreaks str
             val isSame = strVec = vec
           in
             if isSame then true else raise Empty
           end)
      , true
      , rope
      )
end
