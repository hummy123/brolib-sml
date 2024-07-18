structure CompareToRope =
struct
  fun compareTxns arr =
    Vector.foldli
      (fn (idx, (pos, delNum, insStr), (rope, gapBuffer)) =>
         let
           val _ = print ("idx: " ^ Int.toString idx ^ "\n")
           val oldRope = rope
           val strSize = String.size insStr

           val rope =
             if delNum > 0 then TinyRope.delete (pos, delNum, rope) else rope
           val rope =
             if strSize > 0 then TinyRope.insert (pos, insStr, rope) else rope

           val gapBuffer =
             if delNum > 0 then LineGap.delete (pos, delNum, gapBuffer)
             else gapBuffer
           val gapBuffer =
             if strSize > 0 then LineGap.insert (pos, insStr, gapBuffer)
             else gapBuffer

          val _ = LineGap.verifyIndex gapBuffer
          val _ = LineGap.verifyLines gapBuffer

           val ropeString = TinyRope.toString rope
           val gapBufferString = LineGap.toString gapBuffer
         in
           if ropeString = gapBufferString then
             (rope, gapBuffer)
           else
             let
               val _ = print
                 ("difference detected at txn number: " ^ (Int.toString idx)
                  ^ "\n")
               val txn = String.concat
                 [ "offending txn: \n"
                 , "pos: "
                 , Int.toString pos
                 , ", delNum: "
                 , Int.toString delNum
                 , ", insStr: |"
                 , insStr
                 , "|\n"
                 ]
               val _ = print txn

               val _ = print "before offending string: \n"
               val _ = print (TinyRope.toString oldRope)
               val _ = print "\n"

               val _ = print "rope string: \n"
               val _ = print (ropeString ^ "\n")
               val _ = print "gap string: \n"
               val _ = print (gapBufferString ^ "\n")
               val _ = raise Empty
             in
               (rope, gapBuffer)
             end
         end) (TinyRope.empty, LineGap.empty) arr

  fun main () =
    let
      val (rope, gap) = compareTxns SvelteComponent.txns
      val _ = print "string contents are equal\n"
      val _ = LineGap.verifyLines gap
    (*
    val _ = compareTxns Rust.txns
    val _ = compareTxns Seph.txns
    val _ = compareTxns Automerge.txns
    *)
    in
      ()
    end

  val _ = main ()
end
