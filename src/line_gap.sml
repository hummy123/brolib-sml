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
                  helpCountLineBreaks (pos - 1, pos :: acc, str)
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
    { idx = 0
    , leftStrings = []
    , rightStrings = []
    , line = 0
    , leftLines = []
    , rightLines = []
    }

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

  (* Binary search. If value isn't found, returns the value before it. *)
  local
    fun reverseLinearSearch (findNum, idx, lines) =
      if idx < 0 then
        idx
      else
        let
          val curVal = Vector.sub (lines, idx)
        in
          if curVal < findNum then idx
          else reverseLinearSearch (findNum, idx, lines)
        end

    fun helpBinSearch (findNum, lines, low, high) =
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
          reverseLinearSearch (findNum, mid, lines)
      end
  in
    fun binSearch (findNum, lines) =
      if Vector.length lines = 0 then 0
      else helpBinSearch (findNum, lines, 0, Vector.length lines - 1)
  end

  (* Binary search. If value isn't found, returns the value after it. *)
  local
    fun forwardLinearSearch (findNum, idx, lines) =
      if idx = Vector.length lines then
        idx
      else
        let
          val curVal = Vector.sub (lines, idx)
        in
          if curVal > findNum then idx
          else forwardLinearSearch (findNum, idx, lines)
        end

    fun helpBinSearch (findNum, lines, low, high) =
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
          forwardLinearSearch (findNum, mid, lines)
      end
  in
    fun forwardBinSearch (findNum, lines) =
      if Vector.length lines = 0 then 0
      else helpBinSearch (findNum, lines, 0, Vector.length lines - 1)
  end

  (* Insert function and helper functions for it. *)
  local
    fun insWhenIdxAndCurIdxAreEqual
      ( newString
      , newLines
      , curIdx
      , curLine
      , leftStrings
      , leftLines
      , rightStrings
      , rightLines
      ) : t =
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
              , leftStrings = newLeftString
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
          val midpoint = binSearch (String.size strSub1 - 1, leftLinesHd)
        in
          if
            isThreeInLimit (strSub1, newString, strSub2, leftLinesHd, newLines)
          then
            (* Join three strings together. *)
            let
              val joinedString = String.concat [strSub1, newString, strSub2]
              val joinedLines =
                if Vector.length leftLinesHd > 0 then
                  Vector.tabulate
                    ( Vector.length leftLinesHd + Vector.length newLines
                    , fn idx =>
                        if idx <= midpoint then
                          Vector.sub (leftLinesHd, idx)
                        else if idx <= midpoint + Vector.length newLines then
                          Vector.sub (newLines, (idx - midpoint) - 1)
                          + String.size strSub1
                        else
                          Vector.sub
                            (leftLinesHd, (idx - Vector.length newLines))
                          + String.size newString
                    )
                else
                  Vector.map (fn el => el + String.size strSub1) newLines
            in
              { idx = curIdx + String.size newString
              , line = curLine + Vector.length newLines
              , leftStrings = joinedString :: leftStringsTl
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
            if midpoint >= 0 then
              (* Implicit: a binSearch match was found. *)
              let
                val newLeftLinesLength = midpoint + 1 + Vector.length newLines
                val newLeftLines =
                  Vector.tabulate (newLeftLinesLength, fn idx =>
                    if idx <= midpoint then
                      Vector.sub (leftLinesHd, idx)
                    else
                      Vector.sub (newLines, idx - (midpoint + 1))
                      + String.size strSub1)

                val newRightLines =
                  Vector.tabulate
                    ( (Vector.length leftLinesHd - midpoint) - 1
                    , fn idx =>
                        Vector.sub (leftLinesHd, idx + midpoint + 1)
                        - String.size strSub1
                    )
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
            else
              let
                (* No binSearch result found. *)
                val newLeftLines =
                  Vector.map (fn el => el + String.size strSub1) newLines
                val newRightLines =
                  Vector.map (fn idx => idx - String.size strSub1) leftLinesHd
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
              val newLeftLines =
                if midpoint >= 0 andalso Vector.length leftLinesHd > 0 then
                  let
                    val newLeftLines = VectorSlice.slice
                      (leftLinesHd, 0, SOME (midpoint + 1))
                  in
                    VectorSlice.vector newLeftLines
                  end
                else
                  Vector.fromList []

              val newRightLines =
                Vector.tabulate
                  ( (Vector.length leftLinesHd - Vector.length newLeftLines)
                    + Vector.length newLines
                  , fn idx =>
                      if idx < Vector.length newLines then
                        Vector.sub (newLines, idx)
                      else
                        Vector.sub
                          ( leftLinesHd
                          , (idx - Vector.length newLines)
                            + Vector.length newLeftLines
                          ) - String.size strSub1 + String.size newString
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
              val lineSub1 =
                if midpoint >= 0 andalso Vector.length leftLinesHd > 0 then
                  let
                    val lineSub1 = VectorSlice.slice
                      (leftLinesHd, 0, SOME (midpoint + 1))
                  in
                    VectorSlice.vector lineSub1
                  end
                else
                  Vector.fromList []

              val lineSub2Length =
                Vector.length leftLinesHd - Vector.length lineSub1
              val lineSub2 = Vector.tabulate (lineSub2Length, fn idx =>
                Vector.sub (leftLinesHd, idx + Vector.length lineSub1)
                - String.size strSub1)
            in
              { idx = prevIdx + String.size strSub1 + String.size newString
              , line =
                  (curLine - String.size leftStringsHd) + midpoint
                  + Vector.length newLines
              , leftStrings = newString :: strSub1 :: leftStringsTl
              , leftLines = newLines :: lineSub1 :: leftLinesTl
              , rightStrings = strSub2 :: rightStrings
              , rightLines = lineSub2 :: rightLines
              }
            end
        end

    fun moveLeftAndIns
      ( idx
      , newString
      , newLines: int vector
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
                 ( rightStringsHd :: rightStringsTl
                 , rightLinesHd :: rightLinesTl
                 ) =>
                   if
                     isInLimit
                       ( leftStringsHd
                       , rightStringsHd
                       , leftLinesHd
                       , rightLinesHd
                       )
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
                                   ( rightLinesHd
                                   , idx - Vector.length leftLinesHd
                                   ) + String.size leftStringsHd
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
                ( Vector.length rightLinesHd + Vector.length newLines
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
          val midpoint = binSearch (String.size strSub1 - 1, rightLinesHd)
        in
          if
            isThreeInLimit (strSub1, newString, strSub2, rightLinesHd, newLines)
          then
            (* Join three strings together. *)
            let
              val newRightStringsHd =
                String.concat [strSub1, newString, strSub2]
              val newRightLinesHd =
                if Vector.length rightLinesHd > 0 then
                  Vector.tabulate
                    ( Vector.length rightLinesHd + Vector.length newLines
                    , fn idx =>
                        if idx <= midpoint then
                          Vector.sub (rightLinesHd, idx)
                        else if idx <= midpoint + Vector.length newLines then
                          Vector.sub (newLines, (idx - midpoint) - 1)
                          + String.size strSub1
                        else
                          Vector.sub
                            (rightLinesHd, (idx - Vector.length newLines))
                          + String.size newString
                    )
                else
                  Vector.map (fn el => el + String.size strSub1) newLines
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
            if midpoint >= 0 then
              let
                (* Implicit: a binSearch match was found. *)
                val newLeftStringsHd = strSub1 ^ newString
                val newLeftLinesLength = midpoint + 1 + Vector.length newLines
                val newLeftLinesHd =
                  Vector.tabulate (newLeftLinesLength, fn idx =>
                    if idx <= midpoint then
                      Vector.sub (rightLinesHd, idx)
                    else
                      Vector.sub (newLines, idx - (midpoint + 1))
                      + String.size strSub1)

                val newRightLinesHd =
                  Vector.tabulate
                    ( (Vector.length rightLinesHd - midpoint) - 1
                    , fn idx =>
                        Vector.sub (rightLinesHd, idx + midpoint + 1)
                        - String.size strSub1
                    )
              in
                { idx = curIdx + String.size newLeftStringsHd
                , line = curLine + Vector.length newLeftLinesHd
                , leftStrings = newLeftStringsHd :: leftStrings
                , leftLines = newLeftLinesHd :: leftLines
                , rightStrings = strSub2 :: rightStringsTl
                , rightLines = newRightLinesHd :: rightLinesTl
                }
              end
            else
              let
                (* No binSearch match found. *)
                val newLeftStringsHd = strSub1 ^ newString
                val newLeftLinesHd =
                  Vector.map (fn el => el + String.size strSub1) newLines
                val newRightLinesHd =
                  Vector.map (fn idx => idx - String.size strSub1) rightLinesHd
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
              val newLeftLinesHd =
                if midpoint >= 0 then
                  let
                    val newLeftLinesHd = VectorSlice.slice
                      (rightLinesHd, 0, SOME (midpoint + 1))
                  in
                    VectorSlice.vector newLeftLinesHd
                  end
                else
                  Vector.fromList []

              val newRightStringsHd = newString ^ strSub2
              val newRightLinesHd =
                Vector.tabulate
                  ( (Vector.length newLines + Vector.length rightLinesHd)
                    - Vector.length newLeftLinesHd
                  , fn idx =>
                      if idx < Vector.length newLines then
                        Vector.sub (newLines, idx)
                      else
                        (Vector.sub
                           ( rightLinesHd
                           , (idx - Vector.length newLines)
                             + Vector.length newLeftLinesHd
                           ) - String.size strSub1) + String.size newString
                  )
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
              val lineSub1 =
                if midpoint >= 0 andalso Vector.length rightLinesHd > 0 then
                  let
                    val lineSub1 = VectorSlice.slice
                      (rightLinesHd, 0, SOME (midpoint + 1))
                  in
                    VectorSlice.vector lineSub1
                  end
                else
                  Vector.fromList []

              val lineSub2Length =
                Vector.length rightLinesHd - Vector.length lineSub1
              val lineSub2 = Vector.tabulate (lineSub2Length, fn idx =>
                Vector.sub (rightLinesHd, idx + Vector.length lineSub1)
                - String.size strSub1)
            in
              { idx = curIdx + String.size strSub1 + String.size newString
              , line = curLine + Vector.length newLines + Vector.length lineSub1
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
                       ( leftStringsHd
                       , rightStringsHd
                       , leftLinesHd
                       , rightLinesHd
                       )
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
                                   ( rightLinesHd
                                   , idx - Vector.length leftLinesHd
                                   ) + String.size leftStringsHd
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
  in
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

  (* Delete function and helper functions for it. *)
  fun deleteRightFromHere
    ( origIdx
    , origLine
    , moveIdx
    , finish
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    ) =
    case (rightStrings, rightLines) of
      (rightStringsHd :: rightStringsTl, rightLinesHd :: rightLinesTl) =>
        let
          val nextIdx = moveIdx + String.size rightStringsHd
        in
          if nextIdx < finish then
            (* Keep moving right. *)
            deleteRightFromHere
              ( origIdx
              , origLine
              , nextIdx
              , finish
              , leftStrings
              , leftLines
              , rightStringsTl
              , rightLinesTl
              )
          else if nextIdx > finish then
            (* Base case: delete from the start of this string and stop moving. *)
            let
              (* Delete part of string. *)
              val newStrStart = finish - curIdx
              val newStr = String.substring
                (hd, newStrStart, String.size hd - newStrStart)

              (* Delete from line vector if we need to. *)
              val newLines =
                if Vector.length rightLinesHd > 0 then
                  let
                    val lineDeleteEnd = binSearch
                      (String.size newStr - 1, rightLinesHd)
                  in
                    if lineDeleteEnd >= 0 then
                      Vector.tabulate
                        ( Vector.length rightLinesHd - lineDeleteEnd
                        , fn idx =>
                            Vector.sub (rightLinesHd, idx + lineDeleteEnd + 1)
                            - newStrStart
                        )
                    else
                      (* Subtract by difference in length, which is same as
                       * newStrStart. *)
                      Vector.map (fn idx => idx - newStrStart) rightLinesHd
                  end
                else
                  rightLinesHd (* empty vector *)
            in
              { idx = origIdx
              , line = origLine
              , leftStrings = leftStrings
              , leftLines = leftLines
              , rightStrings = newStr :: rightStringTl
              , rightLines = newLines :: rightLinesTl
              }
            end
          else
            (* Delete this node fully, but delete no further. *)
            { idx = origIdx
            , line = origLine
            , leftStrings = leftStrings
            , leftLines = leftLines
            , rightStrings = rightStringsTl
            , rightLines = rightLinesTl
            }
        end

  fun moveRightAndDelete
    ( start
    , finish
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
          val nextIdx = curIdx + String.size rightLinesHd
        in
          if nextIdx < start then
            (* Keep moving right. 
             * Complicated code below is an optimsation to reduce number of
             * elements in the gap buffer.
             * If we can join left head with right head while staying in limit, then
             * do so; else, just cons as we move. *)
            (case (leftStrings, leftLines) of
               (leftStringsHd :: leftStringsTl, leftLinesHd :: leftLinesTl) =>
                 if
                   isInLimit
                     (leftStringsHd, rightStringsHd, leftLinesHd, rightLinesHd)
                 then
                   (* We can join the heads while staying in limit, so do so. *)
                   let
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
                     val newLeftStrings = newLeftStringsHd :: leftStringsTl
                     val newLeftLines = newLeftLinesHd :: leftLinesTl
                   in
                     moveRightAndDelete
                       ( start
                       , finish
                       , nextIdx
                       , curLine + Vector.length rightLinesHd
                       , newLeftStrings
                       , newLeftLines
                       , rightStringsTl
                       , rightLinesTl
                       )
                   end
                 else
                   (* Can't join heads while staying in limit, so just cons. *)
                   moveRightAndDelete
                     ( start
                     , finish
                     , nextIdx
                     , curLine + Vector.length rightLinesHd
                     , rightStringsHd :: leftStrings
                     , rightLinesHd :: leftLines
                     , rightStringsTl
                     , rightLinesTl
                     )
             | (_, _) =>
                 (* Can't join heads while staying in limit, so just cons. *)
                 moveRightAndDelete
                   ( start
                   , finish
                   , nextIdx
                   , curLine + Vector.length rightLinesHd
                   , rightStringsHd :: leftStrings
                   , rightLinesHd :: leftLines
                   , rightStringsTl
                   , rightLinesTl
                   ))
          else if nextIdx > start then
            if nextIdx < finish then
              (* Start deleting from the end of this string,
               * and then continue deleting rightwards. *)
              let
                val length = start - curIdx
                val newString = String.substring (rightStringsHd, 0, length)

                val lineDeleteEnd = binSearch
                  (String.size newString - 1, rightLinesHd)
                val newLines =
                  if lineDeleteEnd >= 0 then
                    let
                      val slice = VectorSlice.slice
                        (rightLinesHd, 0, SOME (lineDeleteEnd + 1))
                    in
                      VectorSlice.vector slice
                    end
                  else
                    Vector.fromList []
                val nextLine = curLine + Vector.length newLines
              in
                (* Try joining new string with left head if possible. *)
                (case (leftStrings, leftLines) of
                   (leftStringsHd :: leftStringsTl, leftLinesHd :: leftLinesTl) =>
                     if
                       isInLimit
                         (newString, leftStringsHd, newLines, leftLinesHd)
                     then
                       (* Join new string with left head. *)
                       let
                         val newLeftStringsHd = leftStringsHd ^ newString
                         val newLeftLinesHd =
                           Vector.tabulate
                             ( Vector.length leftLinesHd
                               + Vector.length newLines
                             , fn idx =>
                                 if idx < Vector.length leftLinesHd then
                                   Vector.sub (leftLinesHd, idx)
                                 else
                                   Vector.sub
                                     (newLines, idx - Vector.length leftLinesHd)
                                   + String.size leftStringHd
                             )
                       in
                         deleteRightFromHere
                           ( nextIdx
                           , nextLine
                           , finish
                           , newLeftStringsHd :: leftStringsTl
                           , newLeftLinesHd :: leftLinesTl
                           , rightStringsTl
                           , rightLinesTl
                           )
                       end
                     else
                       (* Can't join new string with left head
                       * while staying in limit, so just cons. *)
                       deleteRightFromHere
                         ( nextIdx
                         , nextLine
                         , finish
                         , newString :: leftStrings
                         , newLines :: leftLinesHd
                         , rightStringsTl
                         , rightLinesTl
                         )
                 | (_, _) =>
                     deleteRightFromHere
                       ( nextIdx
                       , nextLine
                       , finish
                       , newString :: leftStrings
                       , newLines :: leftLinesHd
                       , rightStringsTl
                       , rightLinesTl
                       ))
              end
            else if nextIdx > finish then
              (* Base case: delete from the middle part of this string. *)
              let
                val sub1Length = start - curIdx
                val sub1 = String.substring (rightStringsHd, 0, sub1Length)
                val sub1LineEnd = binSearch (String.size sub1 - 1, rightLinesHd)
                val sub1Lines =
                  if sub1LineEnd >= 0 then
                    let
                      val slice = VectorSlice.slice
                        (rightLinesHd, 0, SOME (sub1LineEnd + 1))
                    in
                      VectorSlice.vector slice
                    end
                  else
                    Vector.fromList []

                val sub2Start = finish - curIdx
                val sub2 = String.substring
                  ( rightStringsHd
                  , sub2Start
                  , String.size rightStringsHd - sub2Start
                  )
                val sub2LineStart = forwardBinSearch (sub2Start, rightLinesHd)
                val sub2Lines =
                  if sub2LineStart < Vector.length rightLinesHd then
                    Vector.tabulate
                      ( Vector.length rightLinesHd - Vector.length sub1Lines
                      , fn idx =>
                          Vector.sub (rightLinesHd, idx + sub2LineStart)
                          - (String.size rightStringsHd - String.size sub2)
                      )
                  else
                    Vector.fromList []
              in
                (case (leftStrings, leftLines) of
                   (leftStringsHd :: leftStringsTl, leftLinesHd :: leftLinesTl) =>
                     let
                       val isLeftInLimit =
                         isInLimit (leftStringsHd, sub1, leftLinesHd, sub1Lines)
                       val isRightInLimit =
                         isInLimit
                           (rightStringsHd, sub2, rightLinesHd, sub2Lines)
                     in
                       if isLeftInLimit andalso isRightInLimit then
                         let
                           val newLeftStringsHd = leftStringsHd ^ sub1
                           val newLinesLinesHd =
                             Vector.tabulate
                               ( Vector.length leftLinesHd
                                 + Vector.length sub1Lines
                               , fn idx =>
                                   if idx < Vector.length leftLinesHd then
                                     Vector.sub (leftLinesHd, idx)
                                   else
                                     Vector.sub
                                       ( sub1Lines
                                       , idx - Vector.length leftLinesHd
                                       ) + String.size leftStringsHd
                               )

                           val newRightStringsHd = sub2 ^ rightStringsHd
                           val newRightLinesHd =
                             Vector.tabulate
                               ( Vector.length rightLinesHd
                                 + Vector.length sub2Lines
                               , fn idx =>
                                   if idx < Vector.length sub2Lines then
                                     Vector.sub (sub2Lines, idx)
                                   else
                                     Vector.sub
                                       ( rightLinesHd
                                       , idx - Vector.length sub2Lines
                                       ) + String.size sub2
                               )
                         in
                           { idx = curIdx + String.size sub1
                           , line = curLine + Vector.length sub1Lines

                           , leftStrings = newLeftStringsHd :: leftStringsTl
                           , leftLines = newLeftLinesHd :: leftLinesTl
                           , rightStrings = newRightStringsHd :: rightStringsTl
                           , rightLines = newRightLinesHd :: rightLinesTl
                           }
                         end
                       else if isLeftInLimit then
                         let
                           val newLeftStringsHd = leftStringsHd ^ sub1
                           val newLinesLinesHd =
                             Vector.tabulate
                               ( Vector.length leftLinesHd
                                 + Vector.length sub1Lines
                               , fn idx =>
                                   if idx < Vector.length leftLinesHd then
                                     Vector.sub (leftLinesHd, idx)
                                   else
                                     Vector.sub
                                       ( sub1Lines
                                       , idx - Vector.length leftLinesHd
                                       ) + String.size leftStringsHd
                               )
                         in
                           { idx = curIdx + String.size sub1
                           , line = curLine + Vector.length sub1Lines
                           , leftStrings = newLeftStringsHd :: leftStringsTl
                           , leftLines = newLeftLinesHd :: leftLinesTl
                           , rightStrings = sub2 :: rightStrings
                           , rightLines = sub2Lines :: rightLines
                           }
                         end
                       else if isRightInLimit then
                         let
                           val newRightStringsHd = sub2 ^ rightStringsHd
                           val newRightLinesHd =
                             Vector.tabulate
                               ( Vector.length rightLinesHd
                                 + Vector.length sub2Lines
                               , fn idx =>
                                   if idx < Vector.length sub2Lines then
                                     Vector.sub (sub2Lines, idx)
                                   else
                                     Vector.sub
                                       ( rightLinesHd
                                       , idx - Vector.length sub2Lines
                                       ) + String.size sub2
                               )
                         in
                           { idx = curIdx + String.size sub1
                           , line = curLine + Vector.length sub1Lines

                           , leftStrings = sub1 :: leftStrings
                           , leftLines = sub1Lines :: leftLines
                           , rightStrings = newRightStringsHd :: rightStringsTl
                           , rightLines = newRightLinesHd :: rightLinesTl
                           }
                         end
                       else
                         { idx = curIdx + String.size sub1
                         , line = curLine + Vector.length sub1Lines
                         , leftStrings = sub1 :: leftStrings
                         , leftLines = sub1Lines :: leftLines
                         , rightStrings = sub2 :: rightStrings
                         , rightLines = sub2Lines :: rightLines
                         }
                     end
                 | (_, _) =>
                     { idx = curIdx + String.size sub1
                     , line = curLine + Vector.length sub1Lines
                     , leftStrings = sub1 :: leftStrings
                     , leftLines = sub1Lines :: leftLines
                     , rightStrings = sub2 :: rightStrings
                     , rightLines = sub2Lines :: rightLines
                     })
              end
            else
              (* nextIdx = finish 
               * Base case: delete from middle to end of this string, keeping start. *)
              let
                val strLength = start - curIdx
                val str = String.substring (rightStringsHd, 0, strLength)
                val midpoint = binSearch (String.size str - 1, rightLinesHd)
                val newLeftLines =
                  if midpoint >= 0 then
                    let
                      val slice = VectorSlice.slice
                        (leftLines, 0, SOME (midpoint + 1))
                    in
                      VectorSlice.vector slice
                    end
                  else
                    Vector.fromList []
              in
                { idx = curIdx + strLength
                , line = curLine + Vector.length newLeftLines
                , leftStrings = str :: leftStrings
                , leftLines = newLeftLines :: leftLines
                , rightStrings = rightStringsTl
                , rightLines = rightLinesTl
                }
              end
          else
            (* nextIdx = start
             * Another base case of this function.
             * The start of the deletion range contains the rightStrings/LinesHd,
             * and it may extend beyond the current head. 
             * So pass the rightStringsTl and rightLinesTl to a function that
             * will delete rightwards if it needs to, or else terminates. *)
            deleteRightFromHere
              ( curIdx
              , curLine
              , curIdx
              , finish
              , leftStrings
              , leftLines
              , rightStringsTl
              , rightLinesTl
              )
        end
    | (_, _) =>
        { idx = curIdx
        , line = curLine
        , leftStrings = leftStrings
        , leftLines = leftLines
        , rightStrings = rightStrings
        , rightLines = rightLines
        }

  fun deleteLeftFromHere
    (start, curIdx, curLines, leftStrings, leftLines, rightStrings, rightLines) =
    case (leftStrings, leftLines) of
      (leftStringsHd :: leftStringsTl, leftLinesHd :: leftLinesTl) =>
        let
          val prevIdx = curIdx - String.size leftStringsHd
          val prevLine = curLine - Vector.length leftLinesHd
        in
          if start < prevIdx then
            (* Continue deleting leftward. *)
            deleteLeftFromHere
              ( start
              , prevIdx
              , prevLine
              , leftStringsTl
              , leftLinesTl
              , rightStrings
              , rightLines
              )
          else if start > prevIdx then
            (* Base case: delete end part of this string and return. *)
            let
              val length = start - prevIdx
              val newStr = String.substring (leftStringsHd, 0, length)
              val midpoint = binSearch (String.size newStr - 1, leftLinesHd)
              val newLines =
                let
                  val slice = VectorSlice.slice
                    (leftLinesHd, 0, SOME (midpoint + 1))
                in
                  VectorSlice.vector slice
                end
            in
              { idx = prevIdx + String.size newStr
              , line = prevLine + Vector.length newLines
              , leftStrings = newStr :: leftStringsTl
              , leftLines = newLines :: leftLinesTl
              , rightStrings = rightStrings
              , rightLines = rightLines
              }
            end
          else
            (* start = prevIdx 
             * Base case: Remove leftStrings/LinesHd without removing any further. *)
            { idx = prevIdx
            , line = prevLine
            , leftStrings = leftStringsTl
            , leftLines = leftLinesTl
            , rightStrings = rightStrings
            , rightLines = rightLines
            }
        end
    | (_, _) =>
        { idx = curIdx
        , line = curLine
        , leftStrings = leftStrings
        , leftLines = leftLines
        , rightStrings = rightStrings
        , rightLines = rightLines
        }

  fun deleteFromLetAndRight
    ( start
    , finish
    , curIdx
    , curLine
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    ) =
    let
      val
        { idx = curIdx
        , line = curLine
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        } = deleteRightFromHere
        ( curIdx
        , curLine
        , curIdx
        , finish
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        )
    in
      deleteLeftFromHere
        ( start
        , curIdx
        , curLine
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        )
    end

  fun moveLeftAndDelete
    ( start
    , finish
    , curIdx
    , curLine
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    ) =
    case (leftStrings, leftLines) of
      (leftStringsHd :: leftStringsTl, leftLinesHd :: leftLinesTl) =>
        let
          val prevIdx = curIdx - String.size leftStringsHd
        in
          if prevIdx > finish then
            (* Have to continue moving leftwards. 
             * Case statement below is an optimisation attempt: 
             * We are trying to join strings and line-vectors while staying in
             * limit if this is possible while staying in limit.
             * If this is not possible, we just cons instead. *)
            (case (rightStrings, rightLines) of
               (rightStringsHd :: rightStringsTl, rightLinesHd :: rightLinesTl) =>
                 if
                   isInLimit
                     (leftStringsHd, rightStringsHd, leftLinesHd, rightLinesHd)
                 then
                   (* Can join while staying in limit, so do join. *)
                   let
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
                     val newRightStrings = newRightStringsHd :: rightStringsTl
                     val newRightLines = newRightLinesHd :: rightLinesTl
                   in
                     moveLeftAndDelete
                       ( start
                       , finish
                       , prevIdx
                       , curLine - Vector.length leftLinesHd
                       , leftStringsTl
                       , leftLinesTl
                       , newRightStrings
                       , newRightLines
                       )
                   end
                 else
                   (* Cannot join while staying in limit, so don't. *)
                   moveLeftAndDelete
                     ( start
                     , finish
                     , prevIdx
                     , curLine - Vector.length leftLinesHd
                     , leftStringsTl
                     , leftLinesTl
                     , leftStringsHd :: rightStrings
                     , leftLinesHd :: rightLines
                     )
             | (_, _) =>
                 (* Base case: reached empty list while trying to move leftwards.
                  * Cannot do anything so just return. *)
                 { idx = 0
                 , line = 0
                 , leftStrings = leftStrings
                 , leftLines = leftLines
                 , rightStrings = rightStrings
                 , rightLines = rightLines
                 })
          else if prevIdx < finish then
            if prevIdx > start then
              (* Delete from start point of this string,
               * and then call function to continue deleting leftwards. *)
              let
                val stringStart = finish - prevIdx
                val newString = String.substring
                  ( leftStringsHd
                  , stringStart
                  , String.size leftStringsHd - stringStart
                  )
                val newLines =
                  let
                    val midpoint = binSearch
                      (String.size newString - 1, leftLinesHd)
                  in
                    if midpoint >= 0 then
                      let
                        val slice = VectorSlice.slice
                          (leftLinesHd, 0, SOME (midpoint + 1))
                      in
                        VectorSlice.vector slice
                      end
                    else
                      Vector.fromList []
                  end
                val newRightStrings = newString :: rightStrings
                val newRightLines = newLines :: rightLines
                val prevLine = curLine - Vector.length leftLinesHd
              in
                deleteLeftFromHere
                  ( start
                  , prevIdx
                  , prevLine
                  , leftStringsTl
                  , leftLinesTl
                  , newRightStrings
                  , newRightLines
                  )
              end
            else if prevIdx < start then
              (* We want to delete in the middle of leftStringsHd. 
               * We also have to delete in the middle of leftLinesHd in order to
               * do this. *)
              let
                val sub1Length = start - prevIdx
                val sub1 = String.substring (leftStringsHd, 0, sub1Length)
                val sub2Start = finish - prevIdx
                val sub2 = String.substring
                  ( leftStringsHd
                  , sub2Start
                  , String.size leftStringsHd - sub2Start
                  )

                val sub1Lines =
                  let
                    val midpoint = binSearch (String.size sub1 - 1, leftLinesHd)
                  in
                    if midpoint >= 0 then
                      let
                        val slice = VectorSlice.slice
                          (leftLinesHd, 0, SOME (midpoint + 1))
                      in
                        VectorSlice.vector slice
                      end
                    else
                      Vector.fromList []
                  end

                val sub2Lines =
                  let
                    val midpoint = forwardBinSearch (sub2Start, leftLinesHd)
                  in
                    if midpoint < Vector.length leftLinesHd then
                      Vector.tabulate
                        ( Vector.length leftLinesHd - midpoint
                        , fn idx =>
                            Vector.sub (leftLinesHd, idx + midpoint) - sub2Start
                        )
                    else
                      Vector.fromList []
                  end
              in
                { idx = prevIdx + String.size sub1
                , line =
                    (curLine - Vector.length leftLinesHd)
                    + Vector.length sub1Lines
                , leftStrings = sub1 :: leftStringsTl
                , leftLines = sub1Lines :: leftLinesTl
                , rightStrings = sub2 :: rightStrings
                , rightLines = sub2Lines :: rightLines
                }
              end
            else
              (* prevIdx = start 
               * We want to delete from the start of this string and stop. *)
              let
                val strStart = finish - prevIdx
                val str = String.substring
                  ( leftStringsHd
                  , strStart
                  , String.size leftStringsHd - strStart
                  )
                val lines =
                  let
                    val lineStart = forwardBinSearch (strStart, leftLinesHd)
                  in
                    if lineStart < Vector.length leftLinesHd then
                      Vector.tabulate
                        ( Vector.length leftLinesHd - lineStart
                        , fn idx =>
                            Vector.sub (leftLinesHd, idx + lineStart) - strStart
                        )
                    else
                      Vector.fromList []
                  end
              in
                { idx = prevIdx
                , line = (curLine - Vector.length leftLinesHd) + String.size str
                , leftStrings = str :: leftStringsTl
                , leftLines = lines :: leftLinesTl
                , rightStrings = rightStrings
                , rightLines = rightLines
                }
              end
          else
            (* prevIdx = finish 
             * We need to call a function that will start deleting from prevIdx. 
             * Optimsation: Try joining leftStrings/LinesHd with
             * rightStrings/LinesHd if possible while staying in limit. *)
            (case (rightStrings, rightLines) of
               (rightStringsHd :: rightStringsTl, rightLinesHd :: rightLinesTl) =>
                 if
                   isInLimit
                     (leftStringsHd, rightStringsHd, leftLinesHd, rightLinesHd)
                 then
                   (* Can join while staying in limit. *)
                   let
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
                     deleteLeftFromHere
                       ( start
                       , prevIdx
                       , curLine - Vector.length leftLinesHd
                       , leftStringsTl
                       , leftLinesTl
                       , newRightStringsHd :: rightStringsTl
                       , newRightLinesHd :: rightLinesTl
                       )
                   end
                 else
                   (* Cannot join while staying in limit. *)
                   deleteLeftFromHere
                     ( start
                     , prevIdx
                     , curLine - Vector.length leftLinesHd
                     , leftStringsTl
                     , leftLinesTl
                     , leftStringsHd :: rightStrings
                     , leftLinesHd :: rightLines
                     )
             | (_, _) =>
                 (* Left strings and lines are empty, so just return. *)
                 { idx = 0
                 , line = 0
                 , leftStrings = []
                 , leftLines = []
                 , rightStrings = rightStrings
                 , rightLines = rightLines
                 })
        end
    | (_, _) =>
        (* Can't move further leftward so just return. *)
        { idx = 0
        , line = 0
        , leftStrings = []
        , leftLines = []
        , rightStrings = rightStrings
        , rightLines = rightLines
        }

  (* TEST CODE *)
  local
    fun lineBreaksToString vec =
      (Vector.foldr (fn (el, acc) => Int.toString el ^ ", " ^ acc) "" vec)
      ^ "\n"

    fun checkLineBreaks (v1, v2) =
      if v1 = v2 then
        ()
      else
        let
          val _ = print ("broken: " ^ (lineBreaksToString v1))
          val _ = print ("fixed: " ^ (lineBreaksToString v2))
        in
          ()
        end

    fun goToStart (leftStrings, leftLines, accStrings, accLines) =
      case (leftStrings, leftLines) of
        (lsHd :: lsTl, llHd :: llTl) =>
          goToStart (lsTl, llTl, lsHd :: accStrings, llHd :: accLines)
      | (_, _) => (accStrings, accLines)

    fun verifyLineList (strings, lines) =
      case (strings, lines) of
        (strHd :: strTl, lHd :: lTl) =>
          let
            val checkLines = countLineBreaks strHd
          in
            if checkLines = lHd then
              verifyLineList (strTl, lTl)
            else
              let
                val _ = print "line metadata is incorrect\n"
                val _ = checkLineBreaks (lHd, checkLines)
              in
                raise Empty
              end
          end
      | (_, _) => print "verified lines; no problems\n"
  in
    fun verifyLines (buffer: t) =
      let
        val (strings, lines) =
          goToStart
            ( #leftStrings buffer
            , #leftLines buffer
            , #rightStrings buffer
            , #rightLines buffer
            )
      in
        verifyLineList (strings, lines)
      end
  end
end
