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

  fun lineBreaksToString vec =
    (Vector.foldr (fn (el, acc) => Int.toString el ^ ", " ^ acc) "" vec) ^ "\n"

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

  fun checkLineBreaksWithString (dir, str, lines) =
    if countLineBreaks str <> lines then
      let
        val _ = print (dir ^ "\n")
        val _ = print ("broken: " ^ lineBreaksToString lines)
        val _ = print ("fixed: " ^ (lineBreaksToString (countLineBreaks str)))
        val _ = raise Size
      in
        ()
      end
    else
      ()

  fun verifyReturn (buffer: t) =
    let
      val _ =
        case (#leftStrings buffer, #leftLines buffer) of
          (sHd :: _, lHd :: _) =>
            let val _ = checkLineBreaksWithString ("left", sHd, lHd)
            in ()
            end
        | (_, _) => ()

      val _ =
        case (#rightStrings buffer, #rightLines buffer) of
          (sHd :: _, lHd :: _) =>
            let val _ = checkLineBreaksWithString ("right", sHd, lHd)
            in ()
            end
        | (_, _) => ()

    in
      buffer
    end


  local
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

    fun helpBinSearch (findNum, lines, low, high, prevLow, prevHigh) =
      let
        val mid = low + ((high - low) div 2)
      in
        if high >= low then
          let
            val midVal = Vector.sub (lines, mid)
          in
            if midVal = findNum then
              (print "return 173\n"; mid)
            else if midVal < findNum then
              helpBinSearch (findNum, lines, mid + 1, high, low, high)
            else
              helpBinSearch (findNum, lines, low, mid - 1, low, high)
          end
        else
          let
            val prevLowVal = Vector.sub (lines, prevLow)
            val prevHighVal = Vector.sub (lines, prevHigh)
            val _ = print ("prevLowVal: " ^ Int.toString prevLowVal ^ "\n")
            val _ = print ("prevHighVal: " ^ Int.toString prevHighVal ^ "\n")
            val _ = print ("findNum: " ^ Int.toString findNum ^ "\n")
          in
            (print "return 180\n"; reverseLinearSearch (findNum, mid, lines))
          end
      end
  in
    fun binSearch (findNum, lines) =
      if Vector.length lines = 0 then
        0
      else
        helpBinSearch
          ( findNum
          , lines
          , 0
          , Vector.length lines - 1
          , 0
          , Vector.length lines - 1
          )
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
    ) : t =
    case (leftStrings, leftLines) of
      (strHd :: strTl, lineHd :: lineTl) =>
        if isInLimit (strHd, newString, lineHd, newLines) then
          (* Fits in limit, so we can add to existing string/line vector.*)
          let
            (* VERIFIED TO WORK *)
            val _ = print "line 110\n"
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
            verifyReturn
              { idx = newIdx
              , line = newLine
              , leftStrings = newLeftString
              , leftLines = newLeftLines
              , rightStrings = rightStrings
              , rightLines = rightLines
              }
          end
        else
          let
            val _ = print "line 137\n"
          (* VERIFIED TO WORK *)
          in
            (* Does not fit in limit, so cons instead.*)
            verifyReturn
              { idx = curIdx + String.size newString
              , line = curLine + Vector.length newLines
              , leftStrings = newString :: leftStrings
              , leftLines = newLines :: leftLines
              , rightStrings = rightStrings
              , rightLines = rightLines
              }
          end
    | (_, _) =>
        (* 
         * Because movements between string/line lists in the gap buffer
         * always move together, we know that either list being empty
         * also means that the other one is empty.
         * So we don't need to perform addition or consing.
         *)
        let
          val _ = print "line 156\n"
        (* VERIFIED TO WORK *)
        in
          verifyReturn
            { idx = String.size newString
            , line = Vector.length newLines
            , leftStrings = [newString]
            , leftLines = [newLines]
            , rightStrings = rightStrings
            , rightLines = rightLines
            }
        end

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
      if
        (*
        isInLimit (newString, leftStringsHd, newLines, leftLinesHd) 
        *)
        false
      then
        let
          (* Create new vector, adjusting indices as needed. *)
          (* VERIFIED TO WORK *)
          val _ = print "line 188\n"
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
          verifyReturn
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
        let
          (* VERIFIED TO WORK *)
          val _ = print "line 210\n"
        in
          verifyReturn
            { idx = curIdx + String.size newString
            , line = curLine + Vector.length newLines
            , leftStrings = leftStringsHd :: newString :: leftStringsTl
            , leftLines = leftLinesHd :: newLines :: leftLinesTl
            , rightStrings = rightStrings
            , rightLines = rightLines
            }
        end
    else
      (* Need to insert in the middle of the left list. *)
      let
        (* Get string slices on both sides. *)
        val strLength = idx - prevIdx
        val strSub1 = String.substring (leftStringsHd, 0, strLength)
        val strSub2 = String.substring
          (leftStringsHd, strLength, String.size leftStringsHd - strLength)
        val midpoint = binSearch (String.size strSub1 - 1, leftLinesHd)
        val _ = print ("lines in vec: " ^ lineBreaksToString leftLinesHd ^ "\n")
        val _ = print
          ("vec size:" ^ Int.toString (Vector.length leftLinesHd) ^ "\n")
        val _ = print ("midpoint:" ^ Int.toString (midpoint) ^ "\n")
      in
        if
          (* isThreeInLimit (strSub1, newString, strSub2, leftLinesHd, newLines)
          * *)
          false
        then
          (* Join three strings together. *)
          let
            (* VERIFIED TO WORK *)
            val _ = print "line 233\n"
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
                        Vector.sub (leftLinesHd, (idx - Vector.length newLines))
                        + String.size newString
                  )
              else
                Vector.map (fn el => el + String.size strSub1) newLines
          in
            verifyReturn
              { idx = curIdx + String.size newString
              , line = curLine + Vector.length newLines
              , leftStrings = joinedString :: leftStringsTl
              , leftLines = joinedLines :: leftLinesTl
              , rightStrings = rightStrings
              , rightLines = rightLines
              }
          end
        else if
          false (*
                String.size strSub1 + String.size newString <= stringLimit
                andalso midpoint + Vector.length newLines <= vecLimit
                *)
        then
          (* If we can join newString/lines with sub1 while
           * staying in limit. *)
          let
            (* VERIFIED TO WORK *)
            val _ = print "line 416\n\n\n"
            val _ = print
              ("vector length: " ^ Int.toString (Vector.length newLines) ^ "\n")
            val _ = print ("midpoint: " ^ Int.toString (midpoint) ^ "\n")
            val _ = print ("leftLinesHd: " ^ lineBreaksToString leftLinesHd)

            (* NEWLEFTLINES VERIFIED *)
            val newLeftLines =
              if midpoint >= 0 then
                (* Implicit: a binSearch match was found. *)
                let
                  val newLeftLinesLength = midpoint + 1 + Vector.length newLines
                in
                  Vector.tabulate (newLeftLinesLength, fn idx =>
                    if idx <= midpoint then
                      Vector.sub (leftLinesHd, idx)
                    else
                      Vector.sub (newLines, idx - (midpoint + 1))
                      + String.size strSub1)
                end
              else
                Vector.map (fn el => el + String.size strSub1) newLines

            val _ = print "line 275\n"

            (* NEWRIGHTLINES VERIFIED *)
            val newRightLines =
              if midpoint >= 0 then
                (* Implicit: a binSearch match was found. *)
                Vector.tabulate
                  ( (Vector.length leftLinesHd - midpoint) - 1
                  , fn idx =>
                      Vector.sub (leftLinesHd, idx + midpoint + 1)
                      - String.size strSub1
                  )
              else
                Vector.map (fn idx => idx - String.size strSub1) leftLinesHd
          in
            verifyReturn
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
          (*
            String.size newString + String.size strSub2 <= stringLimit
            andalso
            (Vector.length leftLinesHd - midpoint) + Vector.length newLines
            <= vecLimit
            *)
          false
        then
          (* If we can join newString/line with sub2 while staying
           * in limit. *)
          let
            (* VERIFIED TO WORK *)
            val _ = print "line 478\n"
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
            verifyReturn
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
            (* VERIFIED TO WORK *)
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
            verifyReturn
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
               (rightStringsHd :: rightStringsTl, rightLinesHd :: rightLinesTl) =>
                 if false
                   (*
                     isInLimit
                       (leftStringsHd, rightStringsHd, leftLinesHd, rightLinesHd)
                       *) then
                   let
                     (* VERIFIED TO WORK *)
                     val _ = print "line 370\n"
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
        verifyReturn
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
          (* VERIFIED TO WORK *)
          val _ = print "line 474\n"
          val newRightStringsHd = rightStringsHd ^ newString
          val newRightLinesHd = countLineBreaks newRightStringsHd
        in
          verifyReturn
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
        let
          val _ = print "line 498\n"
        in
          verifyReturn
            { idx = curIdx
            , line = curLine
            , leftStrings = leftStrings
            , leftLines = leftLines
            , rightStrings = rightStringsHd :: newString :: rightStringsTl
            , rightLines = rightLinesHd :: newLines :: rightLinesTl
            }
        end
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
          (*
            isThreeInLimit (strSub1, newString, strSub2, rightLinesHd, newLines)
            *)
          true
        then
          (* Join three strings together. *)
          let
            (* VERIFIED TO WORK *)
            val _ = print "line 573\n"
            val newRightStringsHd = String.concat [strSub1, newString, strSub2]

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
            verifyReturn
              { idx = curIdx
              , line = curLine
              , leftStrings = leftStrings
              , leftLines = leftLines
              , rightStrings = newRightStringsHd :: rightStringsTl
              , rightLines = newRightLinesHd :: rightLinesTl
              }
          end
        else if
          (*
            String.size strSub1 + String.size newString <= stringLimit
            andalso midpoint + Vector.length newLines <= vecLimit
            *)
          true
        then
          (* If we can join newString/lines with sub1 while
           * staying in limit. *)
          let
            (* VERIFIED *)
            (* strSub1 ^ newString is placed on the left list. *)
            val _ = print "line 552\n"
            val newLeftStringsHd = strSub1 ^ newString

            val newLeftLinesHd =
              if midpoint >= 0 then
                (* Implicit: a binSearch match was found. *)
                let
                  val newLeftLinesLength = midpoint + 1 + Vector.length newLines
                in
                  Vector.tabulate (newLeftLinesLength, fn idx =>
                    if idx <= midpoint then
                      Vector.sub (rightLinesHd, idx)
                    else
                      Vector.sub (newLines, idx - (midpoint + 1))
                      + String.size strSub1)
                end
              else
                Vector.map (fn el => el + String.size strSub1) newLines

            val _ = print "line 584\n"
            val newRightLinesHd =
              if midpoint >= 0 then
                (* Implicit: a binSearch match was found. *)
                Vector.tabulate
                  ( (Vector.length rightLinesHd - midpoint) - 1
                  , fn idx =>
                      Vector.sub (rightLinesHd, idx + midpoint + 1)
                      - String.size strSub1
                  )
              else
                Vector.map (fn idx => idx - String.size strSub1) rightLinesHd
          in
            verifyReturn
              { idx = curIdx + String.size newLeftStringsHd
              , line = curLine + Vector.length newLeftLinesHd
              , leftStrings = newLeftStringsHd :: leftStrings
              , leftLines = newLeftLinesHd :: leftLines
              , rightStrings = strSub2 :: rightStringsTl
              , rightLines = newRightLinesHd :: rightLinesTl
              }
          end
        else if
          (*
            String.size newString + String.size strSub2 <= stringLimit
            andalso
            (Vector.length rightLinesHd - midpoint) + Vector.length newLines
            <= vecLimit
            *)
          true
        then
          (* If we can join newString/line with sub2 while staying
           * in limit. *)
          let
            (* VERIFIED TO WORK *)
            val _ = print "line 581\n"
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
            verifyReturn
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
            (* VERIFIED TO WORK *)
            val _ = print "line 608\n"
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
            verifyReturn
              { idx = curIdx + String.size strSub1 + String.size newString
              , line =
                  curLine + Vector.length (countLineBreaks newString)
                  + Vector.length lineSub1
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
                   (*
                     isInLimit
                       (leftStringsHd, rightStringsHd, leftLinesHd, rightLinesHd)
                       *)
                   true
                 then
                   let
                     (* VERIFIED TO WORK *)
                     val _ = print "line 650\n"
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
        let
          val _ = print "line 723\n"
        in
          verifyReturn
            { idx = curIdx
            , line = curLine
            , leftStrings = leftStrings
            , leftLines = leftLines
            , rightStrings = [newString]
            , rightLines = [newLines]
            }
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
