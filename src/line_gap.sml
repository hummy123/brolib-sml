signature LINE_GAP =
sig
  type t =
    { idx: int
    , textLength: int
    , leftStrings: string list
    , rightStrings: string list

    , line: int
    , lineLength: int
    , leftLines: int vector list
    , rightLines: int vector list
    }

  val empty: t

  val fromString: string -> t
  val toString: t -> string

  val substring: int * int * t -> string
  val nullSubstring: int * int * t -> string
  val substringWithEnd: int * int * t * string -> string

  val delete: int * int * t -> t
  val insert: int * string * t -> t
  val append: string * t -> t

  val goToStart: t -> t
  val goToEnd: t -> t
  val goToIdx: int * t -> t
  val goToLine: int * t -> t

  val idxToLineNumber: int * t -> int
  val lineNumberToIdx: int * t -> int

  (* for testing *)
  val verifyIndex: t -> unit
  val verifyLines: t -> unit
end

structure LineGap :> LINE_GAP =
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
    , textLength: int
    , leftStrings: string list
    , rightStrings: string list

    , line: int
    , lineLength: int
    , leftLines: int vector list
    , rightLines: int vector list
    }

  val stringLimit = 1024
  val vecLimit = 32

  val empty =
    { idx = 0
    , textLength = 0
    , leftStrings = []
    , rightStrings = []
    , line = 0
    , lineLength = 0
    , leftLines = []
    , rightLines = []
    }

  fun fromString str =
    let
      val linebreaks = countLineBreaks str
    in
      { idx = 0
      , textLength = String.size str
      , leftStrings = []
      , rightStrings = [str]
      , line = 0
      , lineLength = Vector.length linebreaks
      , leftLines = []
      , rightLines = [linebreaks]
      }
    end

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
          else forwardLinearSearch (findNum, idx + 1, lines)
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
        else if mid >= 0 then
          forwardLinearSearch (findNum, mid, lines)
        else
          0
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
      , textLength
      , lineLength
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
              , textLength = textLength
              , line = newLine
              , lineLength = lineLength
              , leftStrings = newLeftString
              , leftLines = newLeftLines
              , rightStrings = rightStrings
              , rightLines = rightLines
              }
            end
          else
            (* Does not fit in limit, so cons instead.*)
            { idx = curIdx + String.size newString
            , textLength = textLength
            , line = curLine + Vector.length newLines
            , lineLength = lineLength
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
          , textLength = textLength
          , line = Vector.length newLines
          , lineLength = lineLength
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
      , textLength
      , lineLength
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
            , textLength = textLength
            , line = curLine + Vector.length newLines
            , lineLength = lineLength
            , leftStrings = (newString ^ leftStringsHd) :: leftStringsTl
            , leftLines = joinedLines :: leftLinesTl
            , rightStrings = rightStrings
            , rightLines = rightLines
            }
          end
        else
          (* Just cons everything; no way we can join while staying in limit. *)
          { idx = curIdx + String.size newString
          , textLength = textLength
          , line = curLine + Vector.length newLines
          , lineLength = lineLength
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
              , textLength = textLength
              , line = curLine + Vector.length newLines
              , lineLength = lineLength
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
                , textLength = textLength
                , line =
                    (curLine - Vector.length leftLinesHd)
                    + Vector.length newLeftLines
                , lineLength = lineLength
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
                , textLength = textLength
                , line =
                    (curLine - Vector.length leftLinesHd)
                    + Vector.length newLeftLines
                , lineLength = lineLength
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
              , textLength = textLength
              , line = (curLine - Vector.length leftLinesHd) + midpoint
              , lineLength = lineLength
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
              , textLength = textLength
              , line =
                  (curLine - String.size leftStringsHd) + midpoint
                  + Vector.length newLines
              , lineLength = lineLength
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
      , textLength
      , lineLength
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
                         , textLength
                         , lineLength
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
                       , textLength
                       , lineLength
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
                     , textLength
                     , lineLength
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
                , textLength
                , lineLength
                )
          end
      | (_, _) =>
          (* Left list is empty, so need to cons or join.
           * Just set left string/list as newString/newLines. *)
          { idx = String.size newString
          , textLength = textLength
          , line = Vector.length newLines
          , lineLength = lineLength
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
      , textLength
      , lineLength
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
            , textLength = textLength
            , line = curLine
            , lineLength = lineLength
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
          , textLength = textLength
          , line = curLine
          , lineLength = lineLength
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
              , textLength = textLength
              , line = curLine
              , lineLength = lineLength
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
                , textLength = textLength
                , line = curLine + Vector.length newLeftLinesHd
                , lineLength = lineLength
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
                , textLength = textLength
                , line = curLine + Vector.length newLeftLinesHd
                , lineLength = lineLength
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
              , textLength = textLength
              , line = curLine + Vector.length newLeftLinesHd
              , lineLength = lineLength
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
              , textLength = textLength
              , line = curLine + Vector.length newLines + Vector.length lineSub1
              , lineLength = lineLength
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
      , textLength
      , lineLength
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
                         , textLength
                         , lineLength
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
                       , textLength
                       , lineLength
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
                     , textLength
                     , lineLength
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
                , textLength
                , lineLength
                )
          end
      | (_, _) =>
          (* Right string/line is empty. *)
          { idx = curIdx
          , textLength = textLength
          , line = curLine
          , lineLength = lineLength
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
      , textLength
      , lineLength
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
          , textLength
          , lineLength
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
          , textLength
          , lineLength
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
          , textLength
          , lineLength
          )
  in
    fun insert (idx, newString, buffer: t) =
      let
        val newLines = countLineBreaks newString
        val newTextLength = #textLength buffer + String.size newString
        val newLineLength = #lineLength buffer + Vector.length newLines
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
          , newTextLength
          , newLineLength
          )
      end
  end

  fun helpGoToEndAndAppend
    ( newString
    , newLines
    , idx
    , leftStrings
    , rightStrings
    , line
    , leftLines
    , rightLines
    , textLength
    , lineLength
    ) =
    case (rightStrings, rightLines) of
      (rStrHd :: rStrTl, rLnHd :: rLnTl) =>
        (* move gap rightwards one node,
         * and join with right head with left if possible *)
        (case (leftStrings, leftLines) of
           (lStrHd :: lStrTl, lLnHd :: lLnTl) =>
             if isInLimit (lStrHd, rStrHd, lLnHd, rLnHd) then
               let
                 val newLstrHd = lStrHd ^ rStrHd
                 val newLlnHd =
                   Vector.tabulate
                     ( Vector.length lLnHd + Vector.length rLnHd
                     , fn lnIdx =>
                         if lnIdx < Vector.length lLnHd then
                           Vector.sub (lLnHd, lnIdx)
                         else
                           Vector.sub (rLnHd, lnIdx - Vector.length lLnHd)
                           + String.size lStrHd
                     )
               in
                 helpGoToEndAndAppend
                   ( newString
                   , newLines
                   , idx + String.size rStrHd
                   , newLstrHd :: lStrTl
                   , rStrTl
                   , line + Vector.length rLnHd
                   , newLlnHd :: lLnTl
                   , rLnTl
                   , textLength
                   , lineLength
                   )
               end
             else
               helpGoToEndAndAppend
                 ( newString
                 , newLines
                 , idx + String.size rStrHd
                 , rStrHd :: leftStrings
                 , rStrTl
                 , line + Vector.length rLnHd
                 , rLnHd :: leftLines
                 , rLnTl
                 , textLength
                 , lineLength
                 )
         | (_, _) =>
             (* left side is empty; we are at start *)
             helpGoToEndAndAppend
               ( newString
               , newLines
               , String.size rStrHd
               , [rStrHd]
               , rStrTl
               , Vector.length rLnHd
               , [rLnHd]
               , rLnTl
               , textLength
               , lineLength
               ))
    | (_, _) =>
        (* we have reached the end, and right side is empty *)
        (case (leftStrings, leftLines) of
           (lStrHd :: lStrTl, lLnHd :: lLnTl) =>
             if isInLimit (lStrHd, newString, lLnHd, newLines) then
               (* join new string and line with left *)
               let
                 val newLstrHd = lStrHd ^ newString
                 val newLlnHd =
                   Vector.tabulate
                     ( Vector.length lLnHd + Vector.length newLines
                     , fn lnIdx =>
                         if lnIdx < Vector.length lLnHd then
                           Vector.sub (lLnHd, lnIdx)
                         else
                           Vector.sub (newLines, lnIdx - Vector.length lLnHd)
                           + String.size lStrHd
                     )
               in
                 { idx = idx + String.size newString
                 , textLength = textLength
                 , line = line + Vector.length newLines
                 , lineLength = lineLength
                 , leftStrings = newLstrHd :: lStrTl
                 , leftLines = newLlnHd :: lLnTl
                 , rightStrings = []
                 , rightLines = []
                 }
               end
             else
               { idx = idx + String.size newString
               , textLength = textLength
               , line = line + Vector.length newLines
               , lineLength = lineLength
               , leftStrings = newString :: leftStrings
               , leftLines = newLines :: leftLines
               , rightStrings = []
               , rightLines = []
               }
         | (_, _) =>
             { idx = idx + String.size newString
             , textLength = textLength
             , line = line + Vector.length newLines
             , lineLength = lineLength
             , leftStrings = newString :: leftStrings
             , leftLines = newLines :: leftLines
             , rightStrings = []
             , rightLines = []
             })

  fun append (newString, buffer) =
    let
      val
        { idx
        , line
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        , textLength
        , lineLength
        } = buffer
      val newTextLength = textLength + String.size newString
      val newLines = countLineBreaks newString
      val newLineLength = lineLength + Vector.length newLines
    in
      helpGoToEndAndAppend
        ( newString
        , newLines
        , idx
        , leftStrings
        , rightStrings
        , line
        , leftLines
        , rightLines
        , newTextLength
        , newLineLength
        )
    end

  (* Delete function and helper functions for it. *)
  local
    fun deleteRightFromHere
      ( origIdx
      , origLine
      , moveIdx
      , finish
      , leftStrings
      , leftLines
      , rightStrings
      , rightLines
      , textLength
      , lineLength
      ) =
      case (rightStrings, rightLines) of
        (rightStringsHd :: rightStringsTl, rightLinesHd :: rightLinesTl) =>
          let
            val nextIdx = moveIdx + String.size rightStringsHd
          in
            if nextIdx < finish then
              (* Remove string/line head and keep moving right. *)
              deleteRightFromHere
                ( origIdx
                , origLine
                , nextIdx
                , finish
                , leftStrings
                , leftLines
                , rightStringsTl
                , rightLinesTl
                , textLength - String.size rightStringsHd
                , lineLength - Vector.length rightLinesHd
                )
            else if nextIdx > finish then
              (* Base case: delete from the start of this string and stop moving. *)
              let
                val oldNodeTextLength = String.size rightStringsHd
                val oldNodeLineLength = Vector.length rightLinesHd

                (* Delete part of string. *)
                val newStrStart = finish - moveIdx
                val newStr = String.substring
                  ( rightStringsHd
                  , newStrStart
                  , String.size rightStringsHd - newStrStart
                  )

                (* Delete from line vector if we need to. *)
                val newLines =
                  if Vector.length rightLinesHd > 0 then
                    let
                      val lineDeleteStart =
                        forwardBinSearch (newStrStart, rightLinesHd)
                    in
                      if lineDeleteStart < Vector.length rightLinesHd then
                        let
                          val lineDeleteLength =
                            Vector.length rightLinesHd - lineDeleteStart
                        in
                          Vector.tabulate (lineDeleteLength, fn idx =>
                            Vector.sub (rightLinesHd, idx + lineDeleteStart)
                            - newStrStart)
                        end
                      else
                        Vector.fromList []
                    end
                  else
                    rightLinesHd (* empty vector *)

                val newNodeTextLength = String.size newStr
                val textLengthDifference = oldNodeTextLength - newNodeTextLength
                val textLength = textLength - textLengthDifference

                val newNodeLineLength = Vector.length newLines
                val lineLengthDifference = oldNodeLineLength - newNodeLineLength
                val lineLength = lineLength - lineLengthDifference
              in
                { idx = origIdx
                , textLength = textLength
                , line = origLine
                , lineLength = lineLength
                , leftStrings = leftStrings
                , leftLines = leftLines
                , rightStrings = newStr :: rightStringsTl
                , rightLines = newLines :: rightLinesTl
                }
              end
            else
              (* Delete this node fully, but delete no further. *)
              { idx = origIdx
              , textLength = textLength - String.size rightStringsHd
              , line = origLine
              , lineLength = lineLength - Vector.length rightLinesHd
              , leftStrings = leftStrings
              , leftLines = leftLines
              , rightStrings = rightStringsTl
              , rightLines = rightLinesTl
              }
          end
      | (_, _) =>
          { idx = origIdx
          , textLength = textLength
          , line = origLine
          , lineLength = lineLength
          , leftStrings = leftStrings
          , leftLines = leftLines
          , rightStrings = []
          , rightLines = []
          }

    fun moveRightAndDelete
      ( start
      , finish
      , curIdx
      , curLine
      , leftStrings: string list
      , leftLines: int vector list
      , rightStrings: string list
      , rightLines: int vector list
      , textLength
      , lineLength
      ) =
      case (rightStrings, rightLines) of
        (rightStringsHd :: rightStringsTl, rightLinesHd :: rightLinesTl) =>
          let
            val nextIdx = curIdx + String.size rightStringsHd
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
                       ( leftStringsHd
                       , rightStringsHd
                       , leftLinesHd
                       , rightLinesHd
                       )
                   then
                     (* We can join the heads while staying in limit, so do so. *)
                     let
                       val newLeftStringsHd = leftStringsHd ^ rightStringsHd
                       val newLeftLinesHd: int vector =
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
                         , textLength
                         , lineLength
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
                       , textLength
                       , lineLength
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
                     , textLength
                     , lineLength
                     ))
            else if nextIdx > start then
              if nextIdx < finish then
                (* Start deleting from the end of this string,
                 * and then continue deleting rightwards. *)
                let
                  val oldNodeTextLength = String.size rightStringsHd
                  val oldNodeLineLength = Vector.length rightLinesHd

                  val length = start - curIdx
                  val newString = String.substring (rightStringsHd, 0, length)

                  val lineDeleteEnd = binSearch
                    (String.size newString - 1, rightLinesHd)
                  val newLines =
                    if Vector.length rightLinesHd = 0 orelse lineDeleteEnd < 0 then
                      Vector.fromList []
                    else
                      let
                        val slice = VectorSlice.slice
                          (rightLinesHd, 0, SOME (lineDeleteEnd + 1))
                      in
                        VectorSlice.vector slice
                      end

                  val newNodeTextLength = String.size newString
                  val textLengthDifference =
                    oldNodeTextLength - newNodeTextLength
                  val textLength = textLength - textLengthDifference

                  val newNodeLineLength = Vector.length newLines
                  val lineLengthDifference =
                    oldNodeLineLength - newNodeLineLength
                  val lineLength = lineLength - lineLengthDifference
                in
                  (* Try joining new string with left head if possible. *)
                  (case (leftStrings, leftLines) of
                     ( leftStringsHd :: leftStringsTl
                     , leftLinesHd :: leftLinesTl
                     ) =>
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
                                       ( newLines
                                       , idx - Vector.length leftLinesHd
                                       ) + String.size leftStringsHd
                               )
                         in
                           (* moveIdx passed as arameter should be 
                            * different from origIdx,
                            * because moveIdx considers range to delete from
                            * while origIdx considers index to return 
                            * once buffer is done deleting. *)
                           deleteRightFromHere
                             ( curIdx + String.size newString
                             , curLine + Vector.length newLines
                             , nextIdx
                             , finish
                             , newLeftStringsHd :: leftStringsTl
                             , newLeftLinesHd :: leftLinesTl
                             , rightStringsTl
                             , rightLinesTl
                             , textLength
                             , lineLength
                             )
                         end
                       else
                         (* Can't join new string with left head
                         * while staying in limit, so just cons. *)
                         deleteRightFromHere
                           ( curIdx + String.size newString
                           , curLine + Vector.length newLines
                           , nextIdx
                           , finish
                           , newString :: leftStrings
                           , newLines :: leftLines
                           , rightStringsTl
                           , rightLinesTl
                           , textLength
                           , lineLength
                           )
                   | (_, _) =>
                       deleteRightFromHere
                         ( curIdx + String.size newString
                         , curLine + Vector.length newLines
                         , nextIdx
                         , finish
                         , newString :: leftStrings
                         , newLines :: leftLines
                         , rightStringsTl
                         , rightLinesTl
                         , textLength
                         , lineLength
                         ))
                end
              else if nextIdx > finish then
                (* Base case: delete from the middle part of this string. *)
                let
                  val oldNodeTextLength = String.size rightStringsHd
                  val oldNodeLineLength = Vector.length rightLinesHd

                  val sub1Length = start - curIdx
                  val sub1 = String.substring (rightStringsHd, 0, sub1Length)
                  val sub1LineEnd = binSearch
                    (String.size sub1 - 1, rightLinesHd)
                  val sub1Lines =
                    if sub1LineEnd < 0 orelse Vector.length rightLinesHd = 0 then
                      Vector.fromList []
                    else
                      let
                        val slice = VectorSlice.slice
                          (rightLinesHd, 0, SOME (sub1LineEnd + 1))
                      in
                        VectorSlice.vector slice
                      end

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
                        ( Vector.length rightLinesHd - sub2LineStart
                        , fn idx =>
                            Vector.sub (rightLinesHd, idx + sub2LineStart)
                            - (String.size rightStringsHd - String.size sub2)
                        )
                    else
                      Vector.fromList []

                  val newNodeTextLength = String.size sub1 + String.size sub2
                  val textLengthDifference =
                    oldNodeTextLength - newNodeTextLength
                  val newTextLength = textLength - textLengthDifference

                  val newNodeLineLength =
                    Vector.length sub1Lines + Vector.length sub2Lines
                  val lineLengthDifference =
                    oldNodeLineLength - newNodeLineLength
                  val newLineLength = lineLength - lineLengthDifference
                in
                  { idx = curIdx + String.size sub1
                  , textLength = newTextLength
                  , line = curLine + Vector.length sub1Lines
                  , lineLength = newLineLength
                  , leftStrings = sub1 :: leftStrings
                  , leftLines = sub1Lines :: leftLines
                  , rightStrings = sub2 :: rightStringsTl
                  , rightLines = sub2Lines :: rightLinesTl
                  }
                end
              else
                (* nextIdx = finish 
                 * Base case: delete from middle to end of this string, keeping start. *)
                let
                  val oldNodeTextLength = String.size rightStringsHd
                  val oldNodeLineLength = Vector.length rightLinesHd

                  val strLength = start - curIdx
                  val str = String.substring (rightStringsHd, 0, strLength)
                  val midpoint = binSearch (String.size str - 1, rightLinesHd)
                  val newLeftLines =
                    if midpoint < 0 orelse Vector.length rightLinesHd = 0 then
                      Vector.fromList []
                    else
                      let
                        val slice = VectorSlice.slice
                          (rightLinesHd, 0, SOME (midpoint + 1))
                      in
                        VectorSlice.vector slice
                      end

                  val newNodeTextLength = String.size str
                  val textLengthDifference =
                    oldNodeTextLength - newNodeTextLength
                  val newTextLength = textLength - textLengthDifference

                  val newNodeLineLength = Vector.length newLeftLines
                  val lineLengthDifference =
                    oldNodeLineLength - newNodeLineLength
                  val newLineLength = lineLength - lineLengthDifference
                in
                  { idx = curIdx + String.size str
                  , textLength = newTextLength
                  , line = curLine + Vector.length newLeftLines
                  , lineLength = newLineLength
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
                ( curIdx + String.size rightStringsHd
                , curLine + Vector.length rightLinesHd
                , nextIdx
                , finish
                , rightStringsHd :: leftStrings
                , rightLinesHd :: leftLines
                , rightStringsTl
                , rightLinesTl
                , textLength
                , lineLength
                )
          end
      | (_, _) =>
          { idx = curIdx
          , textLength = textLength
          , line = curLine
          , lineLength = lineLength
          , leftStrings = leftStrings
          , leftLines = leftLines
          , rightStrings = rightStrings
          , rightLines = rightLines
          }

    fun deleteLeftFromHere
      ( start
      , curIdx
      , curLine
      , leftStrings
      , leftLines
      , rightStrings
      , rightLines
      , textLength
      , lineLength
      ) =
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
                , textLength - String.size leftStringsHd
                , lineLength - Vector.length leftLinesHd
                )
            else if start > prevIdx then
              (* Base case: delete end part of this string and return. *)
              let
                val oldNodeTextLength = String.size leftStringsHd
                val oldNodeLineLength = Vector.length leftLinesHd

                val length = start - prevIdx
                val newStr = String.substring (leftStringsHd, 0, length)
                val newLines =
                  if Vector.length leftLinesHd > 0 then
                    let
                      val midpoint = binSearch
                        (String.size newStr - 1, leftLinesHd)
                      val slice = VectorSlice.slice
                        (leftLinesHd, 0, SOME (midpoint + 1))
                    in
                      VectorSlice.vector slice
                    end
                  else
                    Vector.fromList []

                val newNodeTextLength = String.size newStr
                val textLengthDifference = oldNodeTextLength - newNodeTextLength
                val newTextLength = textLength - textLengthDifference

                val newNodeLineLength = Vector.length newLines
                val lineLengthDifference = oldNodeLineLength - newNodeLineLength
                val newLineLength = lineLength - lineLengthDifference
              in
                { idx = prevIdx + String.size newStr
                , textLength = newTextLength
                , line = prevLine + Vector.length newLines
                , lineLength = newLineLength
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
              , textLength = textLength - String.size leftStringsHd
              , lineLength = lineLength - Vector.length leftLinesHd
              }
          end
      | (_, _) =>
          { idx = curIdx
          , line = curLine
          , leftStrings = leftStrings
          , leftLines = leftLines
          , rightStrings = rightStrings
          , rightLines = rightLines
          , textLength = textLength
          , lineLength = lineLength
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
      , textLength
      , lineLength
      ) =
      let
        val
          { idx = curIdx
          , line = curLine
          , leftStrings
          , leftLines
          , rightStrings
          , rightLines
          , textLength
          , lineLength
          } = deleteRightFromHere
          ( curIdx
          , curLine
          , curIdx
          , finish
          , leftStrings
          , leftLines
          , rightStrings
          , rightLines
          , textLength
          , lineLength
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
          , textLength
          , lineLength
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
      , textLength
      , lineLength
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
                                   ( rightLinesHd
                                   , idx - Vector.length leftLinesHd
                                   ) + String.size leftStringsHd
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
                         , textLength
                         , lineLength
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
                       , textLength
                       , lineLength
                       )
               | (_, _) =>
                   (* Base case: reached empty list while trying to move leftwards.
                    * Cannot do anything so just return. *)
                   moveLeftAndDelete
                     ( start
                     , finish
                     , prevIdx
                     , curLine - Vector.length leftLinesHd
                     , leftStringsTl
                     , leftLinesTl
                     , [leftStringsHd]
                     , [leftLinesHd]
                     , textLength
                     , lineLength
                     ))
            else if prevIdx < finish then
              if prevIdx > start then
                (* Delete from start point of this string,
                 * and then call function to continue deleting leftwards. *)
                let
                  val oldNodeTextLength = String.size leftStringsHd
                  val oldNodeLineLength = Vector.length leftLinesHd

                  val stringStart = finish - prevIdx
                  val newString = String.substring
                    ( leftStringsHd
                    , stringStart
                    , String.size leftStringsHd - stringStart
                    )
                  val newLines =
                    let
                      val midpoint = forwardBinSearch (stringStart, leftLinesHd)
                    in
                      if midpoint >= 0 then
                        Vector.tabulate
                          ( Vector.length leftLinesHd - midpoint
                          , fn idx =>
                              Vector.sub (leftLinesHd, idx + midpoint)
                              - stringStart
                          )
                      else
                        Vector.fromList []
                    end
                  val newRightStrings = newString :: rightStrings
                  val newRightLines = newLines :: rightLines
                  val prevLine = curLine - Vector.length leftLinesHd

                  val newNodeTextLength = String.size newString
                  val textLengthDifference =
                    oldNodeTextLength - newNodeTextLength
                  val textLength = textLength - textLengthDifference

                  val newNodeLineLength = Vector.length newLines
                  val lineLengthDifference =
                    oldNodeLineLength - newNodeLineLength
                  val lineLength = lineLength - lineLengthDifference
                in
                  deleteLeftFromHere
                    ( start
                    , prevIdx
                    , prevLine
                    , leftStringsTl
                    , leftLinesTl
                    , newRightStrings
                    , newRightLines
                    , textLength
                    , lineLength
                    )
                end
              else if prevIdx < start then
                (* We want to delete in the middle of leftStringsHd. 
                 * We also have to delete in the middle of leftLinesHd in order to
                 * do this. *)
                let
                  val oldNodeTextLength = String.size leftStringsHd
                  val oldNodeLineLength = Vector.length leftLinesHd

                  val sub1Length = start - prevIdx
                  val sub1 = String.substring (leftStringsHd, 0, sub1Length)
                  val sub2Start = finish - prevIdx
                  val sub2 = String.substring
                    ( leftStringsHd
                    , sub2Start
                    , String.size leftStringsHd - sub2Start
                    )

                  val sub1Lines =
                    if Vector.length leftLinesHd > 0 then
                      let
                        val midpoint = binSearch
                          (String.size sub1 - 1, leftLinesHd)
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
                    else
                      leftLinesHd

                  val sub2Lines =
                    let
                      val midpoint = forwardBinSearch (sub2Start, leftLinesHd)
                    in
                      if midpoint < Vector.length leftLinesHd then
                        Vector.tabulate
                          ( Vector.length leftLinesHd - midpoint
                          , fn idx =>
                              Vector.sub (leftLinesHd, idx + midpoint)
                              - sub2Start
                          )
                      else
                        Vector.fromList []
                    end

                  val newNodeTextLength = String.size sub1 + String.size sub2
                  val textLengthDifference =
                    oldNodeTextLength - newNodeTextLength
                  val textLength = textLength - textLengthDifference

                  val newNodeLineLength =
                    Vector.length sub1Lines + Vector.length sub2Lines
                  val lineLengthDifference =
                    oldNodeLineLength - newNodeLineLength
                  val lineLength = lineLength - lineLengthDifference
                in
                  { idx = prevIdx + String.size sub1
                  , line =
                      (curLine - Vector.length leftLinesHd)
                      + Vector.length sub1Lines
                  , leftStrings = sub1 :: leftStringsTl
                  , leftLines = sub1Lines :: leftLinesTl
                  , rightStrings = sub2 :: rightStrings
                  , rightLines = sub2Lines :: rightLines
                  , textLength = textLength
                  , lineLength = lineLength
                  }
                end
              else
                (* prevIdx = start 
                 * We want to delete from the start of this string and stop. *)
                let
                  val oldNodeTextLength = String.size leftStringsHd
                  val oldNodeLineLength = Vector.length leftLinesHd

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
                              Vector.sub (leftLinesHd, idx + lineStart)
                              - strStart
                          )
                      else
                        Vector.fromList []
                    end

                  val newNodeTextLength = String.size str
                  val textLengthDifference =
                    oldNodeTextLength - newNodeTextLength
                  val textLength = textLength - textLengthDifference

                  val newNodeLineLength = Vector.length lines
                  val lineLengthDifference =
                    oldNodeLineLength - newNodeLineLength
                  val lineLength = lineLength - lineLengthDifference
                in
                  { idx = prevIdx + String.size str
                  , textLength = textLength
                  , line =
                      (curLine - Vector.length leftLinesHd) + String.size str
                  , lineLength = lineLength
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
                                   ( rightLinesHd
                                   , idx - Vector.length leftLinesHd
                                   ) + String.size leftStringsHd
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
                         , textLength
                         , lineLength
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
                       , textLength
                       , lineLength
                       )
               | (_, _) =>
                   (* Right strings and lines are empty, so can't join. *)
                   deleteLeftFromHere
                     ( start
                     , prevIdx
                     , curLine - Vector.length leftLinesHd
                     , leftStringsTl
                     , leftLinesTl
                     , [leftStringsHd]
                     , [leftLinesHd]
                     , textLength
                     , lineLength
                     ))
          end
      | (_, _) =>
          (* Can't move further leftward so just return. *)
          { idx = 0
          , textLength = textLength
          , line = 0
          , lineLength = lineLength
          , leftStrings = []
          , leftLines = []
          , rightStrings = rightStrings
          , rightLines = rightLines
          }

    fun del
      ( start
      , finish
      , curIdx
      , curLine
      , leftStrings
      , leftLines
      , rightStrings
      , rightLines
      , textLength
      , lineLength
      ) =
      if start > curIdx then
        moveRightAndDelete
          ( start
          , finish
          , curIdx
          , curLine
          , leftStrings
          , leftLines
          , rightStrings
          , rightLines
          , textLength
          , lineLength
          )
      else if start < curIdx then
        if finish <= curIdx then
          moveLeftAndDelete
            ( start
            , finish
            , curIdx
            , curLine
            , leftStrings
            , leftLines
            , rightStrings
            , rightLines
            , textLength
            , lineLength
            )
        else
          deleteFromLetAndRight
            ( start
            , finish
            , curIdx
            , curLine
            , leftStrings
            , leftLines
            , rightStrings
            , rightLines
            , textLength
            , lineLength
            )
      else
        deleteRightFromHere
          ( curIdx
          , curLine
          , curIdx
          , finish
          , leftStrings
          , leftLines
          , rightStrings
          , rightLines
          , textLength
          , lineLength
          )
  in
    fun delete (start, length, buffer: t) =
      if length > 0 then
        del
          ( start
          , start + length
          , #idx buffer
          , #line buffer
          , #leftStrings buffer
          , #leftLines buffer
          , #rightStrings buffer
          , #rightLines buffer
          , #textLength buffer
          , #lineLength buffer
          )
      else
        buffer
  end

  local
    fun consIfNotEmpty (s, acc) =
      if String.size s > 0 then s :: acc else acc

    (* We build up the string list and, at the end,
     * we always make sure to reverse the list too
     * because the order of the list matters for String.concat *)
    fun subRightFromHere (curIdx, finish, right, acc, endWith) =
      case right of
        hd :: tl =>
          let
            val nextIdx = curIdx + String.size hd
          in
            if nextIdx < finish then
              subRightFromHere (curIdx, finish, tl, hd :: acc, endWith)
            else if nextIdx > finish then
              let
                val length = finish - curIdx
                val accHd = String.substring (hd, 0, length)
                val acc = consIfNotEmpty (endWith, accHd :: acc)
              in
                List.rev acc
              end
            else
              (* nextIdx = finish 
               * so add current hd to vec and then concat *)
              let
                val acc = hd :: acc
                val acc = consIfNotEmpty (endWith, acc)
              in
                List.rev acc
              end
          end
      | [] => let val acc = consIfNotEmpty (endWith, acc) in List.rev acc end

    fun moveRightAndSub (start, finish, curIdx, right, endWith) =
      case right of
        hd :: tl =>
          let
            val nextIdx = curIdx + String.size hd
          in
            if nextIdx < start then
              (* continue moving rightwards *)
              moveRightAndSub (start, finish, nextIdx, tl, endWith)
            else if nextIdx > start then
              if nextIdx < finish then
                (* get starting acc, 
                 * and then call subRightFromHere *)
                let
                  val substart = start - curIdx
                  val length = String.size hd - substart
                  val acc = [String.substring (hd, substart, length)]
                  val acc = subRightFromHere (nextIdx, finish, tl, acc, endWith)
                in
                  String.concat acc
                end
              else if nextIdx > finish then
                (* have to get susbstring from middle of this string *)
                let
                  val substart = start - curIdx
                  val subfinish = finish - curIdx
                  val length = subfinish - substart
                  val str = String.substring (hd, substart, length)
                in
                  if String.size endWith > 0 then str ^ endWith else str
                end
              else
                (* have to get substring from middle to end *)
                let
                  val substart = start - curIdx
                  val length = String.size hd - substart
                  val str = String.substring (hd, substart, length)
                in
                  if String.size endWith > 0 then str ^ endWith else str
                end
            else
              (* nextIdx = start
               * so we have to ignore this string
               * and start building acc from tl *)
              let val acc = subRightFromHere (nextIdx, finish, tl, [], endWith)
              in String.concat acc
              end
          end
      | [] =>
          (* if there are no strings to the right,
           * return empty string,
           * as we cannot do much else. *)
          endWith

    fun subLeftFromHere (start, curIdx, left, acc) =
      case left of
        hd :: tl =>
          let
            val prevIdx = curIdx - String.size hd
          in
            if start < prevIdx then
              (* continue *)
              subLeftFromHere (start, prevIdx, tl, hd :: acc)
            else if start > prevIdx then
              (* need to add some part of this string to acc
               * and return *)
              let
                val substart = start - prevIdx
                val length = String.size hd - substart
                val accHd = String.substring (hd, substart, length)
                val acc = accHd :: acc
              in
                String.concat acc
              end
            else
              (* start = prevIdx
               * add hd to acc and return *)
              let val acc = hd :: acc
              in String.concat acc
              end
          end
      | [] => String.concat acc

    fun subFromLeftAndRight (start, finish, curIdx, left, right, endWith) =
      let val acc = subRightFromHere (curIdx, finish, right, [], endWith)
      in subLeftFromHere (start, curIdx, left, acc)
      end

    fun moveLeftAndSub (start, finish, curIdx, left, endWith) =
      case left of
        hd :: tl =>
          let
            val prevIdx = curIdx - String.size hd
          in
            if prevIdx > finish then
              (* continue *)
              moveLeftAndSub (start, finish, prevIdx, tl, endWith)
            else if prevIdx < finish then
              if prevIdx > start then
                (* get initial acc
                 * and continue substring leftwards *)
                let
                  val length = finish - prevIdx
                  val str = String.substring (hd, 0, length)
                  val acc = [str, endWith]
                in
                  subLeftFromHere (start, prevIdx, tl, acc)
                end
              else if prevIdx < start then
                (* we want to return a substring 
                 * extracted from the middle of hd *)
                let
                  val substart = start - prevIdx
                  val subfinish = finish - prevIdx
                  val length = subfinish - substart
                  val str = String.substring (hd, substart, length)
                in
                  if String.size endWith > 0 then str ^ endWith else str
                end
              else
                (* prevIdx = start
                 * we want to return a substring starting from 0 *)
                let
                  val subfinish = finish - prevIdx
                  val length = String.size hd - subfinish
                  val str = String.substring (hd, 0, length)
                in
                  if String.size endWith > 0 then str ^ endWith else str
                end
            else
              (* prevIdx = finish
               * so we want to ignore hd and start
               * subLeftFromHere with an empty list *)
              subLeftFromHere (start, prevIdx, tl, [endWith])
          end
      | [] => endWith

    fun sub (start, finish, curIdx, left, right, endWith) =
      if start > curIdx then
        (* move rightwards to begin getting substring *)
        moveRightAndSub (start, finish, curIdx, right, endWith)
      else if start < curIdx then
        if finish <= curIdx then
          moveLeftAndSub (start, finish, curIdx, left, endWith)
        else
          (* in middle of buffer we want to get substring from *)
          subFromLeftAndRight (start, finish, curIdx, left, right, endWith)
      else
        let
          (* start = curIdx so only need to traverse right *)
          val acc = subRightFromHere (curIdx, finish, right, [], endWith)
        in
          String.concat acc
        end
  in
    fun substringWithEnd (start, length, buffer: t, endWith) =
      let
        val finish = start + length
        val {idx, leftStrings, rightStrings, ...} = buffer
      in
        sub (start, finish, idx, leftStrings, rightStrings, endWith)
      end

    fun nullSubstring (start, length, buffer: t) =
      let
        val finish = start + length
        val {idx, leftStrings, rightStrings, ...} = buffer
      in
        sub (start, finish, idx, leftStrings, rightStrings, "\u0000")
      end

    fun substring (start, length, buffer: t) =
      let
        val finish = start + length
        val {idx, leftStrings, rightStrings, ...} = buffer
      in
        sub (start, finish, idx, leftStrings, rightStrings, "")
      end
  end

  fun helpGoToStart
    ( idx
    , line
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    , textLength
    , lineLength
    ) =
    case (leftStrings, leftLines) of
      (lStrHd :: lStrTl, lLnHd :: lLnTl) =>
        (case (rightStrings, rightLines) of
           (rStrHd :: rStrTl, rLnHd :: rLnTl) =>
             if isInLimit (lStrHd, rStrHd, lLnHd, rLnHd) then
               (* join if possible *)
               let
                 val newRstrHd = lStrHd ^ rStrHd
                 val newRlnHd =
                   Vector.tabulate
                     ( Vector.length lLnHd + Vector.length rLnHd
                     , fn lnIdx =>
                         if lnIdx < Vector.length lLnHd then
                           Vector.sub (lLnHd, lnIdx)
                         else
                           Vector.sub (rLnHd, lnIdx - Vector.length lLnHd)
                           + String.size lStrHd
                     )
               in
                 helpGoToStart
                   ( idx - String.size lStrHd
                   , line - Vector.length lLnHd
                   , lStrTl
                   , lLnTl
                   , newRstrHd :: rStrTl
                   , newRlnHd :: rLnTl
                   , textLength
                   , lineLength
                   )
               end
             else
               helpGoToStart
                 ( idx - String.size lStrHd
                 , line - Vector.length lLnHd
                 , lStrTl
                 , lLnTl
                 , lStrHd :: rightStrings
                 , lLnHd :: rightLines
                 , textLength
                 , lineLength
                 )
         | (_, _) =>
             (* rightStrings and rightLines are both empty *)
             helpGoToStart
               ( idx - String.size lStrHd
               , line - Vector.length lLnHd
               , lStrTl
               , lLnTl
               , [lStrHd]
               , [lLnHd]
               , textLength
               , lineLength
               ))
    | (_, _) =>
        (* left strings are empty, meaning we are at start and can return *)
        { idx = idx
        , textLength = textLength
        , line = line
        , lineLength = lineLength
        , leftStrings = []
        , leftLines = []
        , rightStrings = rightStrings
        , rightLines = rightLines
        }

  fun goToStart (buffer: t) =
    let
      val
        { idx
        , line
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        , textLength
        , lineLength
        } = buffer
    in
      helpGoToStart
        ( idx
        , line
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        , textLength
        , lineLength
        )
    end

  fun helpGoToEnd
    ( idx
    , line
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    , textLength
    , lineLength
    ) =
    case (rightStrings, rightLines) of
      (rStrHd :: rStrTl, rLnHd :: rLnTl) =>
        (case (leftStrings, leftLines) of
           (lStrHd :: lStrTl, lLnHd :: lLnTl) =>
             if isInLimit (lStrHd, rStrHd, lLnHd, rLnHd) then
               (* join if possible *)
               let
                 val newLstrHd = lStrHd ^ rStrHd
                 val newLlnHd =
                   Vector.tabulate
                     ( Vector.length lLnHd + Vector.length rLnHd
                     , fn lnIdx =>
                         if lnIdx < Vector.length lLnHd then
                           Vector.sub (lLnHd, lnIdx)
                         else
                           Vector.sub (rLnHd, lnIdx - Vector.length lLnHd)
                           + String.size lStrHd
                     )
               in
                 helpGoToEnd
                   ( idx + String.size rStrHd
                   , line + Vector.length rLnHd
                   , newLstrHd :: lStrTl
                   , newLlnHd :: lLnTl
                   , rStrTl
                   , rLnTl
                   , textLength
                   , lineLength
                   )
               end
             else
               helpGoToEnd
                 ( idx + String.size rStrHd
                 , line + Vector.length rLnHd
                 , rStrHd :: leftStrings
                 , rLnHd :: leftLines
                 , rStrTl
                 , rLnTl
                 , textLength
                 , lineLength
                 )
         | (_, _) =>
             (* rightStrings and rightLines are both empty *)
             helpGoToEnd
               ( idx + String.size rStrHd
               , line + Vector.length rLnHd
               , rStrHd :: leftStrings
               , rLnHd :: leftLines
               , rStrTl
               , rLnTl
               , textLength
               , lineLength
               ))
    | (_, _) =>
        (* rightStrings strings are empty, meaning we are at end and can return *)
        { idx = idx
        , textLength = textLength
        , line = line
        , lineLength = lineLength
        , leftStrings = leftStrings
        , leftLines = leftLines
        , rightStrings = []
        , rightLines = []
        }

  fun goToEnd (buffer: t) =
    let
      val
        { idx
        , line
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        , textLength
        , lineLength
        } = buffer
    in
      helpGoToEnd
        ( idx
        , line
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        , textLength
        , lineLength
        )
    end

  (* function to abstract leftwards movement.
   * if the left hd and the right hd can be joined in one node
   * during movement, while staying in limit, then join and move.
   * Else, move without joining.
   * The code to do this is a bit boiler-plate heavy
   * so it has been abstracted to a reusable function. 
   *
   * The last parameter, fGoLeft, is the function to return to
   * after the leftwards movement.
   *
   * The third paremeter, searchTo, is the line number or UTF-8
   * index to search. Since moveLeft is meant to abstract over
   * the search number, this parameter is just passed to fGoLeft.
   * *)
  fun moveLeft
    ( idx
    , line
    , searchTo
    , rightStrings
    , rightLines
    , lStrHd
    , lStrTl
    , lLnHd
    , lLnTl
    , textLength
    , lineLength
    , fGoLeft
    ) =
    case (rightStrings, rightLines) of
      (rStrHd :: rStrTl, rLnHd :: rLnTl) =>
        if isInLimit (lStrHd, rStrHd, lLnHd, rLnHd) then
          (* join into a single node before moving *)
          let
            val newRstrHd = lStrHd ^ rStrHd
            val newRlnHd =
              Vector.tabulate
                ( Vector.length lLnHd + Vector.length rLnHd
                , fn lnIdx =>
                    if lnIdx < Vector.length lLnHd then
                      Vector.sub (lLnHd, lnIdx)
                    else
                      Vector.sub (rLnHd, lnIdx - Vector.length lLnHd)
                      + String.size lStrHd
                )
          in
            fGoLeft
              ( idx - String.size lStrHd
              , line - Vector.length lLnHd
              , searchTo
              , lStrTl
              , lLnTl
              , newRstrHd :: rStrTl
              , newRlnHd :: rLnTl
              , textLength
              , lineLength
              )
          end
        else
          (* move without joining *)
          fGoLeft
            ( idx - String.size lStrHd
            , line - Vector.length lLnHd
            , searchTo
            , lStrTl
            , lLnTl
            , lStrHd :: rightStrings
            , lLnHd :: rightLines
            , textLength
            , lineLength
            )
    | (_, _) =>
        (* right side is empty, so just move left without joining *)
        fGoLeft
          ( idx - String.size lStrHd
          , line - Vector.length lLnHd
          , searchTo
          , lStrTl
          , lLnTl
          , [lStrHd]
          , [lLnHd]
          , textLength
          , lineLength
          )

  (* same as moveLeft function, except it move rightwards instead *)
  fun moveRight
    ( idx
    , line
    , searchTo
    , leftStrings
    , leftLines
    , rStrHd
    , rStrTl
    , rLnHd
    , rLnTl
    , textLength
    , lineLength
    , fGoRight
    ) =
    case (leftStrings, leftLines) of
      (lStrHd :: lStrTl, lLnHd :: lLnTl) =>
        if isInLimit (lStrHd, rStrHd, lLnHd, rLnHd) then
          (* can join while staying in limit, so join and move right *)
          let
            val newLstrHd = lStrHd ^ rStrHd
            val newLlnHd =
              Vector.tabulate
                ( Vector.length lLnHd + Vector.length rLnHd
                , fn lnIdx =>
                    if lnIdx < Vector.length lLnHd then
                      Vector.sub (lLnHd, lnIdx)
                    else
                      Vector.sub (rLnHd, lnIdx - Vector.length lLnHd)
                      + String.size lStrHd
                )
          in
            fGoRight
              ( idx + String.size rStrHd
              , line + Vector.length rLnHd
              , searchTo
              , newLstrHd :: lStrTl
              , newLlnHd :: lLnTl
              , rStrTl
              , rLnTl
              , textLength
              , lineLength
              )
          end
        else
          (* cannot join while staying in limit, so just move right *)
          fGoRight
            ( idx + String.size rStrHd
            , line + Vector.length rLnHd
            , searchTo
            , rStrHd :: leftStrings
            , rLnHd :: leftLines
            , rStrTl
            , rLnTl
            , textLength
            , lineLength
            )
    | (_, _) =>
        (* left side is empty, so just move rightwards without joining *)
        fGoRight
          ( String.size rStrHd
          , Vector.length rLnHd
          , searchTo
          , [rStrHd]
          , [rLnHd]
          , rStrTl
          , rLnTl
          , textLength
          , lineLength
          )

  fun helpGoToLineLeft
    ( idx
    , line
    , searchLine
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    , textLength
    , lineLength
    ) =
    case (leftStrings, leftLines) of
      (lStrHd :: lStrTl, lLnHd :: lLnTl) =>
        if searchLine >= line - Vector.length lLnHd then
          (* line is at left head, so place it to the right and return *)
          { idx = idx - String.size lStrHd
          , textLength = textLength
          , line = line - Vector.length lLnHd
          , lineLength = lineLength
          , leftStrings = lStrTl
          , leftLines = lLnTl
          , rightStrings = lStrHd :: rightStrings
          , rightLines = lLnHd :: rightLines
          }
        else
          (* move leftwards, joining if possible *)
          moveLeft
            ( idx
            , line
            , searchLine
            , rightStrings
            , rightLines
            , lStrHd
            , lStrTl
            , lLnHd
            , lLnTl
            , textLength
            , lineLength
            , helpGoToLineLeft
            )
    | (_, _) =>
        (* left side is empty, so just return *)
        { idx = idx
        , textLength = textLength
        , line = line
        , lineLength = lineLength
        , leftStrings = []
        , leftLines = []
        , rightStrings = rightStrings
        , rightLines = rightLines
        }

  fun helpGoToLineRight
    ( idx
    , line
    , searchLine
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    , textLength
    , lineLength
    ) =
    case (rightStrings, rightLines) of
      (rStrHd :: rStrTl, rLnHd :: rLnTl) =>
        if line + Vector.length rLnHd >= searchLine then
          (* searchLine is in rStrHd/rLnHd, so return *)
          { idx = idx
          , textLength = textLength
          , line = line
          , lineLength = lineLength
          , leftStrings = leftStrings
          , leftLines = leftLines
          , rightStrings = rightStrings
          , rightLines = rightLines
          }
        else
          (* have to move rightwards *)
          moveRight
            ( idx
            , line
            , searchLine
            , leftStrings
            , leftLines
            , rStrHd
            , rStrTl
            , rLnHd
            , rLnTl
            , textLength
            , lineLength
            , helpGoToLineRight
            )
    | (_, _) =>
        (* right side is empty, so just return *)
        { idx = idx
        , textLength = textLength
        , line = line
        , lineLength = lineLength
        , leftStrings = leftStrings
        , leftLines = leftLines
        , rightStrings = []
        , rightLines = []
        }

  fun goToLine (searchLine, buffer: t) =
    let
      val
        { idx
        , line
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        , textLength
        , lineLength
        } = buffer
    in
      (* we compare current line with searchLine - 1
       * because if searchLine - 1 is here,
       * that means we can access the linebreak 
       * that starts searchLine *)
      if searchLine - 1 < line then
        helpGoToLineLeft
          ( idx
          , line
          , searchLine
          , leftStrings
          , leftLines
          , rightStrings
          , rightLines
          , textLength
          , lineLength
          )
      else if searchLine - 1 > line then
        helpGoToLineRight
          ( idx
          , line
          , searchLine
          , leftStrings
          , leftLines
          , rightStrings
          , rightLines
          , textLength
          , lineLength
          )
      else
        buffer
    end

  fun helpGoToIdxLeft
    ( idx
    , line
    , searchIdx
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    , textLength
    , lineLength
    ) =
    case (leftStrings, leftLines) of
      (lStrHd :: lStrTl, lLnHd :: lLnTl) =>
        if searchIdx < idx - String.size lStrHd then
          (* move leftwards, joining if possible *)
          moveLeft
            ( idx
            , line
            , searchIdx
            , rightStrings
            , rightLines
            , lStrHd
            , lStrTl
            , lLnHd
            , lLnTl
            , textLength
            , lineLength
            , helpGoToIdxLeft
            )
        else
          (* line is at left head, so place it to the right and return *)
          { idx = idx - String.size lStrHd
          , textLength = textLength
          , line = line - Vector.length lLnHd
          , lineLength = lineLength
          , leftStrings = lStrTl
          , leftLines = lLnTl
          , rightStrings = lStrHd :: rightStrings
          , rightLines = lLnHd :: rightLines
          }
    | (_, _) =>
        (* left side is empty, so just return *)
        { idx = idx
        , textLength = textLength
        , line = line
        , lineLength = lineLength
        , leftStrings = []
        , leftLines = []
        , rightStrings = rightStrings
        , rightLines = rightLines
        }

  fun helpGoToIdxRight
    ( idx
    , line
    , searchIdx
    , leftStrings
    , leftLines
    , rightStrings
    , rightLines
    , textLength
    , lineLength
    ) =
    case (rightStrings, rightLines) of
      (rStrHd :: rStrTl, rLnHd :: rLnTl) =>
        if searchIdx > idx + String.size rStrHd then
          (* have to move rightwards *)
          moveRight
            ( idx
            , line
            , searchIdx
            , leftStrings
            , leftLines
            , rStrHd
            , rStrTl
            , rLnHd
            , rLnTl
            , textLength
            , lineLength
            , helpGoToIdxRight
            )
        else
          (* searchLine is in rStrHd/rLnHd, so return *)
          { idx = idx
          , textLength = textLength
          , line = line
          , lineLength = lineLength
          , leftStrings = leftStrings
          , leftLines = leftLines
          , rightStrings = rightStrings
          , rightLines = rightLines
          }
    | (_, _) =>
        (* right side is empty, so just return *)
        { idx = idx
        , textLength = textLength
        , line = line
        , lineLength = lineLength
        , leftStrings = leftStrings
        , leftLines = leftLines
        , rightStrings = []
        , rightLines = []
        }

  fun goToIdx (searchIdx, buffer: t) =
    let
      val
        { idx
        , line
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        , textLength
        , lineLength
        } = buffer
    in
      if searchIdx < idx then
        helpGoToIdxLeft
          ( idx
          , line
          , searchIdx
          , leftStrings
          , leftLines
          , rightStrings
          , rightLines
          , textLength
          , lineLength
          )
      else if searchIdx > idx then
        helpGoToIdxRight
          ( idx
          , line
          , searchIdx
          , leftStrings
          , leftLines
          , rightStrings
          , rightLines
          , textLength
          , lineLength
          )
      else
        buffer
    end

  fun idxToLineNumberLeft (findIdx, curIdx, curLine, leftStrings, leftLines) =
    case (leftStrings, leftLines) of
      (shd :: stl, lhd :: ltl) =>
        let
          val prevIdx = curIdx - String.size shd
        in
          if findIdx = prevIdx then
            curLine - Vector.length lhd
          else if findIdx > prevIdx then
            (* bin search vector *)
            if Vector.length lhd = 0 then
              curLine
            else
              let
                val prevLine = curLine - Vector.length lhd
                val relativeIdx = findIdx - prevIdx - 1
                val relativeLine = binSearch (relativeIdx, lhd) + 1
              in
                prevLine + relativeLine
              end
          else
            let val prevLine = curLine - Vector.length lhd
            in idxToLineNumberLeft (findIdx, prevIdx, prevLine, stl, ltl)
            end
        end
    | (_, _) => 0

  fun idxToLineNumberRight (findIdx, curIdx, curLine, rightStrings, rightLines) =
    case (rightStrings, rightLines) of
      (shd :: stl, lhd :: ltl) =>
        let
          val nextIdx = curIdx + String.size shd
        in
          if findIdx = nextIdx then
            curLine + Vector.length lhd
          else if findIdx < nextIdx then
            if Vector.length lhd = 0 then
              curLine
            else
              let
                val relativeIdx = findIdx - curIdx - 1
                val relativeLine = binSearch (relativeIdx, lhd) + 1
              in
                curLine + relativeLine
              end
          else
            let val nextLine = curLine + Vector.length lhd
            in idxToLineNumberRight (findIdx, nextIdx, nextLine, stl, ltl)
            end
        end
    | (_, _) => curLine

  fun idxToLineNumber (findIdx, buffer: t) =
    let
      val
        { idx = curIdx
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        , line = curLine
        , ...
        } = buffer
    in
      if findIdx < curIdx then
        idxToLineNumberLeft (findIdx, curIdx, curLine, leftStrings, leftLines)
      else if findIdx > curIdx then
        idxToLineNumberRight
          (findIdx, curIdx, curLine, rightStrings, rightLines)
      else
        curLine
    end

  fun lineNumberToIdxLeft (findLine, curIdx, curLine, leftStrings, leftLines) =
    case (leftStrings, leftLines) of
      (shd :: stl, lhd :: ltl) =>
        let
          val prevLine = curLine - Vector.length lhd
          val prevIdx = curIdx - String.size shd
        in
          if findLine >= prevLine then
            let val relativeLine = findLine - prevLine - 1
            in Vector.sub (lhd, relativeLine) + prevIdx
            end
          else
            lineNumberToIdxLeft (findLine, prevIdx, prevLine, stl, ltl)
        end
    | (_, _) => 0

  fun lineNumberToIdxRight (findLine, curIdx, curLine, rightStrings, rightLines) =
    case (rightStrings, rightLines) of
      (shd :: stl, lhd :: ltl) =>
        let
          val nextLine = curLine + Vector.length lhd
        in
          if findLine <= nextLine then
            let val relativeLine = findLine - curLine - 1
            in Vector.sub (lhd, relativeLine) + curIdx
            end
          else
            lineNumberToIdxRight
              (findLine, curIdx + String.size shd, nextLine, stl, ltl)
        end
    | (_, _) => curIdx

  fun lineNumberToIdx (findLine, buffer: t) =
    let
      val
        { idx = curIdx
        , line = curLine
        , leftStrings
        , leftLines
        , rightStrings
        , rightLines
        , ...
        } = buffer
    in
      if findLine - 1 < curLine then
        lineNumberToIdxLeft (findLine, curIdx, curLine, leftStrings, leftLines)
      else
        lineNumberToIdxRight
          (findLine, curIdx, curLine, rightStrings, rightLines)
    end

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

    fun isLineListCorrect (strings, lines) =
      case (strings, lines) of
        (strHd :: strTl, lHd :: lTl) =>
          let
            val checkLines = countLineBreaks strHd
          in
            if checkLines = lHd then
              isLineListCorrect (strTl, lTl)
            else
              let
                val _ = print "line metadata is incorrect\n"
                val _ = checkLineBreaks (lHd, checkLines)
              in
                false
              end
          end
      | (_, _) => (print "verified lines; no problems\n"; true)
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

        val lineListIsCorrect = isLineListCorrect (strings, lines)
        val lineLengthIsCorrect = let val lines = Vector.concat lines
                                  in Vector.length lines = #lineLength buffer
                                  end
        val () =
          if lineLengthIsCorrect then () else print "line length is incorrect\n"
      in
        if lineLengthIsCorrect andalso lineListIsCorrect then ()
        else raise Fail ""
      end
  end

  local
    fun calcIndexList (accIdx, lst) =
      case lst of
        [] => accIdx
      | hd :: tl => calcIndexList (String.size hd + accIdx, tl)

    fun calcIndexStart lst = calcIndexList (0, lst)
  in
    fun verifyIndex (buffer: t) =
      let
        val bufferIdx = #idx buffer
        val correctIdx = calcIndexStart (#leftStrings buffer)
        val idxIsCorrect = bufferIdx = correctIdx

        val {rightLines, rightStrings, ...} = goToStart buffer

        val textLength = #textLength buffer
        val correctTextLength = String.size (String.concat rightStrings)
        val textLengthIsCorrect = textLength = correctTextLength

        val lineLength = #lineLength buffer
        val correctLineLength = Vector.length (Vector.concat rightLines)
        val lineLengthIsCorrect = lineLength = correctLineLength

        val _ =
          if idxIsCorrect then
            print "idx is correct\n"
          else
            let
              val msg = String.concat
                [ "idx is incorrect;"
                , "bufferIdx: "
                , Int.toString bufferIdx
                , "; correctIdx: "
                , Int.toString correctIdx
                , "\n"
                ]
            in
              print msg
            end

        val _ =
          if textLengthIsCorrect then
            print "textLength is correct\n"
          else
            let
              val msg = String.concat
                [ "text length is incorrect;"
                , "text length: "
                , Int.toString textLength
                , "; correct length: "
                , Int.toString correctTextLength
                , "\n"
                ]
            in
              print msg
            end

        val _ =
          if lineLengthIsCorrect then
            print "lineLength is correct\n"
          else
            let
              val msg = String.concat
                [ "line length is incorrect;"
                , "line length: "
                , Int.toString lineLength
                , "; correct length: "
                , Int.toString correctLineLength
                , "\n"
                ]
            in
              print msg
            end

        val () = print "\n"
      in
        if textLengthIsCorrect andalso idxIsCorrect andalso lineLengthIsCorrect then
          ()
        else
          raise Fail "either index or idx metadata or text length is incorrect"
      end
  end
end
