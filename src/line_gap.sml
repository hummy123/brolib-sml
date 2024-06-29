structure LineGap =
struct
  local
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
  in
    fun countLineBreaks str =
      helpCountLineBreaks (String.size str - 1, [], str)
  end

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

  local
    fun helpBinSearch (findNum, lines, low, high) =
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
                helpBinSearch (findNum, lines, mid + 1, high)
              else
                helpBinSearch (findNum, lines, low, mid - 1)
            end
          else
            mid
        end
  in
    fun binSearch (findNum, lines) =
      helpBinSearch (findNum, lines, 0, Vector.length lines)
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
            val newLinesHd =
              Vector.tabulate
                ( Vector.length lineHd + Vector.length newLines
                , fn idx =>
                    if idx < Vector.length lineHd then
                      Vector.sub (lineHd, idx)
                    else
                      Vector.sub (newLines, idx - Vector.length lineHd)
                      + String.size strHd
                )
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
    , leftStringsHd
    , leftStringsTl
    , leftLinesHd
    , leftLinesTl
    ) : t =
    if idx = prevIdx then
      (* Need to insert at the start of the left list. *)
      if isInLimit (newString, leftStringsHd, newLines, leftLinesHd) then
        let
          (* Create new vector, adjusting indices as needed. *)
          val joinedLines =
            Vector.tabulate
              ( Vector.length newLines + Vector.length leftLinesHd
              , fn idx =>
                  if idx < Vector.length newLines then
                    Vector.sub (newLines, idx)
                  else
                    Vector.sub (leftLinesHd, idx - Vector.length newLines)
                    + String.size newString
              )
        in
          { idx = curIdx + String.size newString
          , line = curLine + Vector.length newLines
          , leftStrings = (newString ^ leftStringsHd) :: leftStringsTl
          , leftLines = joinedLines :: leftLinesTl
          , rightStrings = rightStrings
          , rightLines = rightLines
          }
        end
      else
        (* Just cons everything; no way we can join while staying in limit. *)
        { idx = curIdx + String.size newString
        , line = curLine + Vector.length newLines
        , leftStrings = leftStringsHd :: newString :: leftStringsTl
        , leftLines = leftLinesHd :: newLines :: leftLinesTl
        , rightStrings = rightStrings
        , rightLines = rightLines
        }
    else
      (* Need to insert in the middle of the left list. *)
      let
        (* Get string slices on both sides. *)
        val strLength = idx - prevIdx
        val strSub1 = String.substring (leftStringsHd, 0, strLength)
        val strSub2 = String.substring
          (leftStringsHd, strLength, String.size leftStringsHd - strLength)
        val midpoint = binSearch (String.size strSub1, leftLinesHd)
      in
        if
          isThreeInLimit (strSub1, newString, strSub2, leftLinesHd, newLines)
        then
          (* Join three strings together. *)
          let
            val joinedLines =
              Vector.tabulate
                ( Vector.length leftLinesHd + Vector.length newLines
                , fn idx =>
                    if idx < midpoint then
                      Vector.sub (leftLinesHd, idx)
                    else if idx < midpoint + Vector.length newLines then
                      Vector.sub (newLines, idx - midpoint)
                      + String.size strSub1
                    else
                      Vector.sub (leftLinesHd, idx - Vector.length newLines)
                      + String.size newString
                )
          in
            { idx = curIdx + String.size newString
            , line = curLine + Vector.length newLines
            , leftStrings =
                String.concat [strSub1, newString, strSub2] :: leftStringsTl
            , leftLines = joinedLines :: leftLinesTl
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
                if idx < midpoint then Vector.sub (leftLinesHd, idx)
                else Vector.sub (newLines, idx - midpoint) + String.size strSub1)
            val newRightLines = VectorSlice.slice (leftLinesHd, midpoint, SOME
              (Vector.length leftLinesHd - midpoint))
            val newRightLines = VectorSlice.vector newRightLines
          in
            { idx = prevIdx + String.size strSub1 + String.size newString
            , line =
                (curLine - Vector.length leftLinesHd)
                + Vector.length newLeftLines
            , leftStrings = (strSub1 ^ newString) :: leftStringsTl
            , leftLines = newLeftLines :: leftLinesTl
            , rightStrings = strSub2 :: rightStrings
            , rightLines = newRightLines :: rightLines
            }
          end
        else if
          String.size newString + String.size strSub2 <= stringLimit
          andalso
          (Vector.length leftLinesHd - midpoint) + Vector.length newLines
          <= vecLimit
        then
          (* If we can join newString/line with sub2 while staying
           * in limit. *)
          let
            val newLeftLines = VectorSlice.slice (leftLinesHd, 0, SOME midpoint)
            val newLeftLines = VectorSlice.vector newLeftLines

            val newRightLines =
              Vector.tabulate
                ( (Vector.length leftLinesHd - midpoint)
                  + Vector.length newLines
                , fn idx =>
                    if idx < Vector.length newLines then
                      Vector.sub (newLines, idx)
                    else
                      (Vector.sub (leftLinesHd, idx - Vector.length newLines)
                       - String.size strSub1) + String.size newString
                )
          in
            { idx = prevIdx + String.size strSub1
            , line = (curLine - Vector.length leftLinesHd) + midpoint
            , leftStrings = strSub1 :: leftStringsTl
            , leftLines = newLeftLines :: leftLinesTl
            , rightStrings = (newString ^ strSub2) :: rightStrings
            , rightLines = newRightLines :: rightLines
            }
          end
        else
          (* Can't join on either side while staying in limit. *)
          let
            val lineSub1 = VectorSlice.slice (leftLinesHd, 0, SOME midpoint)
            val lineSub1 = VectorSlice.vector lineSub1

            val lineSub2 = VectorSlice.slice (leftLinesHd, midpoint, SOME
              (Vector.length leftLinesHd - midpoint))
            val lineSub2 = VectorSlice.vector lineSub2
          in
            { idx = prevIdx + String.size strSub1 + String.size newString
            , line =
                (curLine - String.size leftStringsHd) + midpoint
                + Vector.length newLines
            , leftStrings = newString :: strSub1 :: leftStringsTl
            , leftLines = newLines :: lineSub1 :: leftLinesTl
            , rightStrings = strSub2 :: rightStrings
            , rightLines = lineSub1 :: rightLines
            }
          end
      end

  fun moveLeftAndIns
    ( idx
    , newString
    , newLines
    , curIdx
    , curLine
    , leftStrings: string list
    , leftLines
    , rightStrings
    , rightLines
    ) =
    case (leftStrings, leftLines) of
      (leftStringsHd :: leftStringsTl, leftLinesHd :: leftLinesTl) =>
        let
          val prevIdx = curIdx - String.size leftStringsHd
        in
          if idx < prevIdx then
            (* 
             * Need to move leftward. 
             * The rather complicated code below is an optimisation checking 
             * if we can minimise the number of lists in the gap buffer
             * by concatenating lines/strings together while staying 
             * under the limit. 
             * *)
            (case (rightStrings, rightLines) of
               (rightStringsHd :: rightStringsTl, rightLinesHd :: rightLinesTl) =>
                 if
                   isInLimit
                     (leftStringsHd, rightStringsHd, leftLinesHd, rightLinesHd)
                 then
                   let
                     val prevLine = curLine - Vector.length leftLinesHd
                     val newRightStringsHd = leftStringsHd ^ rightStringsHd
                     val newRightLinesHd =
                       Vector.tabulate
                         ( Vector.length leftLinesHd
                           + Vector.length rightLinesHd
                         , fn idx =>
                             if idx < Vector.length leftLinesHd then
                               Vector.sub (leftLinesHd, idx)
                             else
                               Vector.sub
                                 (rightLinesHd, idx - Vector.length leftLinesHd)
                               + String.size leftStringsHd
                         )
                   in
                     moveLeftAndIns
                       ( idx
                       , newString
                       , newLines
                       , prevIdx
                       , prevLine
                       , leftStringsTl
                       , leftLinesTl
                       , newRightStringsHd :: rightStringsTl
                       , newRightLinesHd :: rightLinesTl
                       )
                   end
                 else
                   moveLeftAndIns
                     ( idx
                     , newString
                     , newLines
                     , prevIdx
                     , curLine - Vector.length leftLinesHd
                     , leftStringsTl
                     , leftLinesTl
                     , leftStringsHd :: rightStrings
                     , leftLinesHd :: rightLines
                     )
             | (_, _) =>
                 moveLeftAndIns
                   ( idx
                   , newString
                   , newLines
                   , prevIdx
                   , curLine - Vector.length newLines
                   , leftStringsTl
                   , leftLinesTl
                   , leftStringsHd :: rightStrings
                   , leftLinesHd :: rightLines
                   ))
          else
            (* Insertion is somewhere between the head of the left list,
             * and the tail of the left list. *)
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
              , leftStringsHd
              , leftStringsTl
              , leftLinesHd
              , leftLinesTl
              )
        end
    | (_, _) =>
        (* Left list is empty, so need to cons or join.
         * Just set left string/list as newString/newLines. *)
        { idx = String.size newString
        , line = Vector.length newLines
        , leftStrings = [newString]
        , leftLines = [newLines]
        , rightStrings = rightStrings
        , rightLines = rightLines
        }

  fun insInRightList
    ( idx
    , newString
    , newLines
    , curIdx
    , curLine
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    , nextIdx
    , rightStringsHd
    , rightStringsTl
    , rightLinesHd: int vector
    , rightLinesTl
    ) : t =
    if idx = nextIdx then
      (* Need to put newString/newLines at the end of the right list's hd. *)
      if isInLimit (newString, rightStringsHd, newLines, rightLinesHd) then
        (* Allocate new string because we can do so while staying in limit. *)
        let
          val newRightStringsHd = rightStringsHd ^ newString
          val newRightLinesHd =
            Vector.tabulate
              ( Vector.length newLines + Vector.length rightLinesHd
              , fn idx =>
                  if idx < Vector.length rightLinesHd then
                    Vector.sub (rightLinesHd, idx)
                  else
                    Vector.sub (newLines, idx - Vector.length rightLinesHd)
                    + String.size rightStringsHd
              )
        in
          { idx = curIdx
          , line = curLine
          , leftStrings = leftStrings
          , leftLines = leftLines
          , rightStrings = newRightStringsHd :: rightStringsTl
          , rightLines = newRightLinesHd :: rightLinesTl
          }
        end
      else
        (* Cons newString and newLines to after-the-head,
         * because we can't join while staying in the limit.*)
        { idx = curIdx
        , line = curLine
        , leftStrings = leftStrings
        , leftLines = leftLines
        , rightStrings = rightStringsHd :: newString :: rightStringsTl
        , rightLines = rightLinesHd :: newLines :: rightLinesTl
        }
    else
      (* Have to split rightStringsHd and rightLinesHd in the middle. *)
      let
        val strLength = idx - curIdx
        val strSub1 = String.substring (rightStringsHd, 0, strLength)
        val strSub2 = String.substring
          (rightStringsHd, strLength, String.size rightStringsHd - strLength)
        val midpoint = binSearch (String.size strSub1, rightLinesHd)
      in
        if
          isThreeInLimit (strSub1, newString, strSub2, rightLinesHd, newLines)
        then
          (* Join three strings together. *)
          let
            val newRightStringsHd = String.concat [strSub1, newString, strSub2]
            val newRightLinesHd =
              Vector.tabulate
                ( Vector.length rightLinesHd + Vector.length newLines
                , fn idx =>
                    if idx < midpoint then
                      Vector.sub (rightLinesHd, idx)
                    else if idx < midpoint + Vector.length newLines then
                      Vector.sub (newLines, idx - midpoint)
                      + String.size strSub1
                    else
                      Vector.sub (rightLinesHd, idx - Vector.length newLines)
                      + String.size newString
                )
          in
            { idx = curIdx
            , line = curLine
            , leftStrings = leftStrings
            , leftLines = leftLines
            , rightStrings = newRightStringsHd :: rightStringsTl
            , rightLines = newRightLinesHd :: rightLinesTl
            }
          end
        else if
          String.size strSub1 + String.size newString <= stringLimit
          andalso midpoint + Vector.length newLines <= vecLimit
        then
          (* If we can join newString/lines with sub1 while
           * staying in limit. *)
          let
            (* strSub1 ^ newString is placed on the left list. *)
            val newLeftStringsHd = strSub1 ^ newString
            val newLeftLinesHd =
              Vector.tabulate (Vector.length newLines + midpoint, fn idx =>
                if idx < midpoint then Vector.sub (rightLinesHd, idx)
                else Vector.sub (newLines, idx - midpoint) + String.size strSub1)

            val newRightLinesHd =
              VectorSlice.slice (rightLinesHd, midpoint, SOME
                (Vector.length rightStringsHd - midpoint))
            val newRightLinesHd = VectorSlice.vector newRightLinesHd
          in
            { idx = curIdx + String.size newLeftStringsHd
            , line = curLine + Vector.length newLeftLinesHd
            , leftStrings = newLeftStringsHd :: leftStrings
            , leftLines = newLeftLinesHd :: leftLines
            , rightStrings = strSub2 :: rightStringsTl
            , rightLines = newRightLinesHd :: rightLinesTl
            }
          end
        else if
          String.size newString + String.size strSub2 <= stringLimit
          andalso
          (Vector.length rightLinesHd - midpoint) + Vector.length newLines
          <= vecLimit
        then
          (* If we can join newString/line with sub2 while staying
           * in limit. *)
          let
            val newRightStringsHd = newString ^ strSub2
            val newRightLinesHd =
              Vector.tabulate
                ( Vector.length newLines + Vector.length rightLinesHd - midpoint
                , fn idx =>
                    if idx < Vector.length newLines then
                      Vector.sub (newLines, idx)
                    else
                      Vector.sub (rightLinesHd, idx - Vector.length newLines)
                      + String.size newString
                )
            val newLeftLinesHd =
              VectorSlice.slice (rightLinesHd, 0, SOME midpoint)
            val newLeftLinesHd = VectorSlice.vector newLeftLinesHd
          in
            { idx = curIdx + String.size strSub1
            , line = curLine + Vector.length newLeftLinesHd
            , leftStrings = strSub1 :: leftStrings
            , leftLines = newLeftLinesHd :: leftLines
            , rightStrings = newRightStringsHd :: rightStringsTl
            , rightLines = newRightLinesHd :: rightLinesTl
            }
          end
        else
          (* Can't join on either side while staying in limit. *)
          let
            val lineSub1 = VectorSlice.slice (rightLinesHd, 0, SOME midpoint)
            val lineSub1 = VectorSlice.vector lineSub1
            val lineSub2 = VectorSlice.slice (rightLinesHd, midpoint, SOME
              (Vector.length rightLinesHd - midpoint))
            val lineSub2 = VectorSlice.vector lineSub2
          in
            { idx = curIdx + String.size strSub1 + String.size newString
            , line = curLine + Vector.length lineSub1 + Vector.length newLines
            , leftStrings = newString :: strSub1 :: leftStrings
            , leftLines = newLines :: lineSub1 :: leftLines
            , rightStrings = strSub2 :: rightStringsTl
            , rightLines = lineSub2 :: rightLinesTl
            }
          end
      end

  fun moveRightAndIns
    ( idx
    , newString
    , newLines
    , curIdx
    , curLine
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    ) =
    case (rightStrings, rightLines) of
      (rightStringsHd :: rightStringsTl, rightLinesHd :: rightLinesTl) =>
        let
          val nextIdx = curIdx + String.size rightStringsHd
        in
          if idx > nextIdx then
            (* Need to move rightward. *)
            (case (leftStrings, leftLines) of
               (leftStringsHd :: leftStringsTl, leftLinesHd :: leftLinesTl) =>
                 if
                   isInLimit
                     (leftStringsHd, rightStringsHd, leftLinesHd, rightLinesHd)
                 then
                   let
                     val nextLine = curLine + Vector.length rightLinesHd
                     val newLeftStringsHd = leftStringsHd ^ rightStringsHd
                     val newLeftLinesHd =
                       Vector.tabulate
                         ( Vector.length leftLinesHd
                           + Vector.length rightLinesHd
                         , fn idx =>
                             if idx < Vector.length leftLinesHd then
                               Vector.sub (leftLinesHd, idx)
                             else
                               Vector.sub
                                 (rightLinesHd, idx - Vector.length leftLinesHd)
                               + String.size leftStringsHd
                         )
                   in
                     moveRightAndIns
                       ( idx
                       , newString
                       , newLines
                       , nextIdx
                       , nextLine
                       , newLeftStringsHd :: leftStringsTl
                       , newLeftLinesHd :: leftLinesTl
                       , rightStringsTl
                       , rightLinesTl
                       )
                   end
                 else
                   moveRightAndIns
                     ( idx
                     , newString
                     , newLines
                     , nextIdx
                     , curLine + Vector.length rightLinesHd
                     , rightStringsHd :: leftStrings
                     , rightLinesHd :: leftLines
                     , rightStringsTl
                     , rightLinesTl
                     )
             | (_, _) =>
                 moveRightAndIns
                   ( idx
                   , newString
                   , newLines
                   , nextIdx
                   , curLine + Vector.length rightLinesHd
                   , rightStringsHd :: leftStrings
                   , rightLinesHd :: leftLines
                   , rightStringsTl
                   , rightLinesTl
                   ))
          else
            (* Need to insert in the middle of the right string's hd. *)
            insInRightList
              ( idx
              , newString
              , newLines
              , curIdx
              , curLine
              , leftStrings
              , leftLines
              , rightStrings
              , rightLines
              , nextIdx
              , rightStringsHd
              , rightStringsTl
              , rightLinesHd
              , rightLinesTl
              )
        end
    | (_, _) =>
        (* Right string/line is empty. *)
        { idx = curIdx
        , line = curLine
        , leftStrings = leftStrings
        , leftLines = leftLines
        , rightStrings = [newString]
        , rightLines = [newLines]
        }

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
      moveLeftAndIns
        ( idx
        , newString
        , newLines
        , curIdx
        , curLine
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        )
    else
      (* idx > curIdx. *)
      moveRightAndIns
        ( idx
        , newString
        , newLines
        , curIdx
        , curLine
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        )

  fun insert (idx, newString, buffer: t) =
    let
      val newLines = countLineBreaks newString
    in
      ins
        ( idx
        , newString
        , newLines
        , #idx buffer
        , #line buffer
        , #leftStrings buffer
        , #leftLines buffer
        , #rightStrings buffer
        , #rightLines buffer
        )
    end
end
