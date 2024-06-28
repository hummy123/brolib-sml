structure LineGap =
struct
  type t =
    { idx: int
    , leftStrings: string list
    , rightStrings: string list

    , line: int
    , leftLines: int vector list
    , rightLines: int vector list
    }

  val stringLimit = 1024
  val vecLimit = 32

  val empty =
    {idx = 0, left = [], right = [], line = 0, leftLines = [], rightLines = []}

  local
    fun helpToString (acc, input) =
      case input of
        hd :: tl => helpToString (hd :: acc, tl)
      | [] => String.concat acc
  in
    fun toString ({leftStrings, rightStrings, ...}: t) =
      helpToString (rightStrings, leftStrings)
  end

  fun isInLimit (s1, s2, v1, v2) =
    String.size s1 + String.size s2 <= stringLimit
    andalso Vector.length v1 + Vector.length v2 <= vecLimit

  fun isThreeInLimit (s1, s2, s3, v1, v2) =
    String.size s1 + String.size s2 + String.size s3 <= stringLimit
    andalso Vector.length v1 + Vector.length v2 <= vecLimit

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

  fun insWhenIdxAndCurIdxAreEqual
    ( newString
    , newLines
    , curIdx
    , curLine
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    ) =
    case (leftStrings, leftLines) of
      (strHd :: strTl, lineHd :: lineTl) =>
        if isInLimit (strHd, newString, lineHd, newLines) then
          (* Fits in limit, so we can add to existing string/line vector.*)
          let
            val newIdx = curIdx + String.size newString
            val newStrHd = strHd ^ newString
            val newLeftString = newStrHd :: strTl
            val newLine = curLine + Vector.length newLines
            val newLinesHd = Vector.concat [lineHd, newLines]
            val newLeftLines = newLinesHd :: lineTl
          in
            { idx = newIdx
            , line = newLine
            , leftStrings = leftStrings
            , leftLines = newLeftLines
            , rightStrings = rightStrings
            , rightLines = rightLines
            }
          end
        else
          (* Does not fit in limit, so cons instead.*)
          { idx = curIdx + String.size newString
          , line = curLine + Vector.length newLines
          , leftStrings = newString :: leftStrings
          , leftLines = newLines :: leftLines
          , rightStrings = rightStrings
          , rightLines = rightLines
          }
    | (_, _) =>
        (* 
         * Because movements between string/line lists in the gap buffer
         * always move together, we know that either list being empty
         * also means that the other one is empty.
         * So we don't need to perform addition or consing.
         *)
        { idx = String.size newString
        , line = Vector.length newLines
        , leftStrings = [newString]
        , leftLines = [newLines]
        , rightStrings = rightStrings
        , rightLines = rightLines
        }

  fun insInLeftList
    ( idx
    , newString
    , newLines
    , curIdx
    , curLine
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    , prevIdx
    , leftStrHd
    , leftStrTl
    , leftLineHd
    , leftLineTl
    ) : t =
    if idx = prevIdx then
      (* Need to insert at the start of the left list. *)
      if isInLimit (newString, leftStrHd, newLines, leftLineHd) then
        let
          (* Create new vector, adjusting indices as needed. *)
          val joinedLines =
            Vector.tabulate
              ( Vector.length newLines + Vector.length leftLineHd
              , fn idx =>
                  if idx < Vector.length newLines then
                    Vector.sub (newLines, idx)
                  else
                    Vector.sub (leftLineHd, idx - Vector.length newLines)
                    + String.size newString
              )
        in
          { idx = curIdx + String.size newString
          , line = curLine + Vector.length newLines
          , leftStrings = (newString ^ leftStrHd) :: leftStrTl
          , leftLines = joinedLines :: leftLineTl
          , rightStrings = rightStrings
          , rightLines = rightLines
          }
        end
      else
        (* Just cons everything; no way we can join while staying in limit. *)
        { idx = curIdx + String.size newString
        , line = curLine + Vector.length newLines
        , leftStrings = leftStrHd :: newString :: leftStrTl
        , leftLines = leftLineHd :: newLines :: leftLineTl
        , rightStrings = rightStrings
        , rightLines = rightLines
        }
    else
      (* Need to insert in the middle of the left list. *)
      let
        (* Get string slices on both sides. *)
        val strLength = idx - prevIdx
        val strSub1 = String.substring (leftStrHd, 0, strLength)
        val strSub2 = String.substring
          (leftStrHd, strLength, String.size leftStrHd - strLength)
        val midpoint =
          binSearch
            (String.size strSub1, leftLineHd, 0, Vector.length leftLineHd)
      in
        if
          isThreeInLimit (strSub1, newString, strSub2, leftLineHd, newLines)
        then
          (* Join three strings together. *)
          let
            val joinedLines =
              Vector.tabulate
                ( Vector.length leftLineHd + Vector.length newLines
                , fn idx =>
                    if idx < midpoint then
                      Vector.sub (leftLineHd, idx)
                    else if idx < midpoint + String.size newString then
                      Vector.sub (newLines, idx - midpoint)
                      - String.size strSub1
                    else
                      Vector.sub (leftLineHd, idx - Vector.length newLines)
                      + String.size newString
                )
          in
            { idx = curIdx + String.size newString
            , line = curLine + Vector.length newLines
            , leftStrings =
                String.concat [strSub1, newString, strSub2] :: leftStrTl
            , leftLines = joinedLines :: leftLineTl
            , rightStrings = rightStrings
            , rightLines = rightLines
            }
          end
        else if
          String.size strSub1 + String.size newString <= stringLimit
          andalso midpoint + Vector.length newLines <= vecLimit
        then
          (* If we can join newString/lines with sub1 while
           * staying in limit. *)
          let
            val newLeftLines =
              Vector.tabulate (midpoint + Vector.length newLines, fn idx =>
                if idx < midpoint then Vector.sub (leftLineHd, idx)
                else Vector.sub (newLines, idx - midpoint) + String.size strSub1)
            val newRightLines = VectorSlice.slice (leftLineHd, midpoint, SOME
              (Vector.length leftLineHd - midpoint))
            val newRightLines = VectorSlice.vector newRightLines
          in
            { idx = prevIdx + String.size strSub1 + String.size newString
            , line =
                (curLine - Vector.length leftLineHd)
                + Vector.length newLeftLines
            , leftStrings = (strSub1 ^ newString) :: leftStrTl
            , leftLines = newLeftLines :: leftLineTl
            , rightStrings = strSub2 :: rightStrings
            , rightLines = newRightLines :: rightLines
            }
          end
        else if
          String.size newString + String.size strSub2 <= stringLimit
          andalso
          (Vector.length leftLineHd - midpoint) + Vector.length newLines
          <= vecLimit
        then
          (* If we can join newString/line with sub2 while staying
           * in limit. *)
          let
            val newLeftLines = VectorSlice.slice (leftLineHd, 0, SOME midpoint)
            val newLeftLines = VectorSlice.vector newLeftLines

            val newRightLines =
              Vector.tabulate
                ( (Vector.length leftLineHd - midpoint) + Vector.length newLines
                , fn idx =>
                    if idx < Vector.length newLines then
                      Vector.sub (newLines, idx)
                    else
                      (Vector.sub (leftLineHd, idx - Vector.length newLines)
                       - String.size strSub1) + String.size newString
                )
          in
            { idx = prevIdx + String.size strSub1
            , line = (curLine - Vector.length leftLineHd) + midpoint
            , leftStrings = strSub1 :: leftStrTl
            , leftLines = newLeftLines :: leftLineTl
            , rightStrings = (newString ^ strSub2) :: rightStrings
            , rightLines = newRightLines :: rightLines
            }
          end
        else
          (* Can't join on either side while staying in limit. *)
          let
            val lineSub1 = VectorSlice.slice (leftLineHd, 0, SOME midpoint)
            val lineSub1 = VectorSlice.vector lineSub1

            val lineSub2 = VectorSlice.slice (leftLineHd, midpoint, SOME
              (Vector.length leftLineHd - midpoint))
            val lineSub2 = VectorSlice.vector lineSub2
          in
            { idx = prevIdx + String.size strSub1 + String.size newString
            , line =
                (curLine - String.size leftStrHd) + midpoint
                + Vector.length newLines
            , leftStrings = newString :: strSub1 :: leftStrTl
            , leftLines = newLines :: lineSub1 :: leftLineTl
            , rightStrings = strSub2 :: rightStrings
            , rightLines = lineSub1 :: rightLines
            }
          end
      end

  fun ins
    ( idx
    , newString
    , newLines
    , curIdx
    , curLine
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    ) : t =
    if curIdx = idx then
      insWhenIdxAndCurIdxAreEqual
        ( newString
        , newLines
        , curIdx
        , curLine
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        )
    else if idx < curIdx then
      (* Check if we need to insert leftward or move the gap buffer left. *)
      case (leftStrings, leftLines) of
        (leftStrHd :: leftStrTl, leftLineHd :: leftLineTl) =>
          let
            val prevIdx = curIdx - String.size leftStrHd
          in
            if idx < prevIdx then
              (* Need to move leftward.
               * The rather complicated code below is an optimisation checking 
               * if we can minimise the number of lists in the gap buffer
               * by concatenating lines/strings together while staying 
               * under the limit. *)
              (case (rightStrings, rightLines) of
                 (rightStrHd :: rightStrTl, rightLineHd :: rightLineTl) =>
                   if isInLimit (leftStrHd, rightStrHd, leftLineHd, rightLineHd) then
                     ins
                       ( idx
                       , newString
                       , newLines
                       , prevIdx
                       , curLine - Vector.length newLines
                       , leftStrTl
                       , leftLineTl
                       , (leftStrHd ^ rightStrHd) :: rightStrTl
                       , (Vector.concat [leftLineHd, rightLineHd] :: rightLineTl)
                       )
                   else
                     ins
                       ( idx
                       , newString
                       , newLines
                       , prevIdx
                       , curLine - Vector.length newLines
                       , leftStrTl
                       , leftLineTl
                       , leftStrHd :: rightStrings
                       , leftLineHd :: rightLines
                       )
               | (_, _) =>
                   ins
                     ( idx
                     , newString
                     , newLines
                     , prevIdx
                     , curLine - Vector.length newLines
                     , leftStrTl
                     , leftLineTl
                     , leftStrHd :: rightStrings
                     , leftLineHd :: rightLines
                     ))
            else
              insInLeftList
                ( idx
                , newString
                , newLines
                , curIdx
                , curLine
                , leftStrings
                , leftLines
                , rightStrings
                , rightLines
                , prevIdx
                , leftStrHd
                , leftStrTl
                , leftLineHd
                , leftLineTl
                )
          end
      | (_, _) =>
          (* The left list is empty, so no need to cons or join.
           * Just set the left values to the newString/newLine. *)
          { idx = String.size newString
          , line = Vector.length newLines
          , leftStrings = [newString]
          , leftLines = [newLines]
          , rightStrings = rightStrings
          , rightLines = rightLines
          }
    else
      raise Empty

end
