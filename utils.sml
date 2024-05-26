fun runTxns arr =
  Vector.foldl
    (fn ((pos, delNum, insStr), rope) =>
       let
         val rope =
           if delNum > 0 then GapBuffer.delete (pos, delNum, rope) else rope
         val strSize = String.size insStr
         val rope =
           if strSize > 0 then GapBuffer.insert (pos, insStr, rope) else rope
       in
         rope
       end) GapBuffer.empty arr

fun runTxnsTime arr =
  let
    val startTime = Time.now ()
    val startTime = Time.toMilliseconds startTime

    val x = runTxns arr

    val endTime = Time.now ()
    val endTime = Time.toMilliseconds endTime
    val timeDiff = endTime - startTime
    val timeDiff = LargeInt.toString timeDiff
    val timeTook = String.concat ["took ", timeDiff, " ms\n"]
    val _ = (print timeTook)
  in
    x
  end

fun compareTxns arr =
  Vector.foldli
    (fn (idx, (pos, delNum, insStr), (rope, gapBuffer)) =>
       let
         val oldRope = rope
         val strSize = String.size insStr

         val rope =
           if delNum > 0 then TinyRope.delete (pos, delNum, rope) else rope
         val rope =
           if strSize > 0 then TinyRope.insert (pos, insStr, rope) else rope

         val gapBuffer =
           if delNum > 0 then GapBuffer.delete (pos, delNum, gapBuffer)
           else gapBuffer
         val gapBuffer =
           if strSize > 0 then GapBuffer.insert (pos, insStr, gapBuffer)
           else gapBuffer

         val ropeString = TinyRope.toString rope
         val gapBufferString = GapBuffer.toString gapBuffer
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
       end) (TinyRope.empty, GapBuffer.empty) arr

fun runToString rope = GapBuffer.toString rope

fun writeFile filename acc =
  let
    val str = String.concatWith "," acc
    val fd = TextIO.openOut filename
    val _ = TextIO.output (fd, str) handle e => (TextIO.closeOut fd; raise e)
    val _ = TextIO.closeOut fd
  in
    ()
  end

fun write (fileName, rope) =
  let
    val str = GapBuffer.toString rope
    val io = TextIO.openOut fileName
    val _ = TextIO.output (io, str)
    val _ = TextIO.closeOut io
  in
    ()
  end

fun runTxnsStats (ins, del, empty, arr) =
  Vector.foldl
    (fn ((pos, delNum, insStr), (buffer, lst)) =>
       let
         val startTime = Time.now ()
         val startTime = Time.toMilliseconds startTime

         val buffer = if delNum > 0 then del (pos, delNum, buffer) else buffer
         val strSize = String.size insStr
         val buffer = if strSize > 0 then ins (pos, insStr, buffer) else buffer

         val endTime = Time.now ()
         val endTime = Time.toMilliseconds endTime
         val timeDiff = endTime - startTime

         val lst = timeDiff :: lst
       in
         (buffer, lst)
       end) (empty, []) arr

fun printListStats (lst, min, max) =
  case lst of
    [] =>
      let
        val _ = print ("minimum time: " ^ LargeInt.toString min ^ "\n")
        val _ = print ("maximum time: " ^ LargeInt.toString max ^ "\n")
        val _ = print "\n"
      in
        ()
      end
  | hd :: tl =>
      let
        val min = LargeInt.min (min, hd)
        val max = LargeInt.max (max, hd)
      in
        printListStats (tl, min, max)
      end

fun runTxnsAndGetStats (ins, del, empty, arr) =
  let
    val (buffer, lst) = runTxnsStats (ins, del, empty, arr)
    val _ = printListStats (lst, LargeInt.fromInt 1000, LargeInt.fromInt ~1000)
  in
    buffer
  end

fun printBufferStats (title, ins, del, empty) =
  let
    val _ = print (title ^ "\n")
    val _ = runTxnsAndGetStats (ins, del, empty, SvelteComponent.txns)
    val _ = runTxnsAndGetStats (ins, del, empty, RustCode.txns)
    val _ = runTxnsAndGetStats (ins, del, empty, SephBlog.txns)
    val _ = runTxnsAndGetStats (ins, del, empty, AutomergePaper.txns)
  in
    ()
  end

fun main () =
  let
    (* Timing benchmarks. *)
    val svelte = runTxnsTime SvelteComponent.txns
    val rust = runTxnsTime RustCode.txns
    val seph = runTxnsTime SephBlog.txns
    val automerge = runTxnsTime AutomergePaper.txns
    val _ = print "\n"

    val _ =
      printBufferStats
        ( "GAP BUFFER STATS: "
        , GapBuffer.insert
        , GapBuffer.delete
        , GapBuffer.empty
        )

    val _ =
      printBufferStats
        ("TINY ROPE STATS: ", TinyRope.insert, TinyRope.delete, TinyRope.empty)

    (* Tests for correctness; will fail if incorrect. *)
    (** Tests for insertion correctness (compare against rope). **)
    val _ = compareTxns SvelteComponent.txns
    val _ = print "svelte test passed\n"

    val _ = compareTxns RustCode.txns
    val _ = print "rust test passed\n"

    val _ = compareTxns SephBlog.txns
    val _ = print "seph test passed\n"

    val _ = compareTxns AutomergePaper.txns
    val _ = print "automerge test passed\n"

    (* Tests for line metadata. *)
    (*
      val _ = Rope.verifyLines svelte
      val _ = Rope.verifyLines rust
      val _ = Rope.verifyLines seph
      val _ = Rope.verifyLines automerge
    *)

    val _ = write ("out/svelte_gap.txt", svelte)
    val _ = write ("out/rust23_gap.txt", rust)
    val _ = write ("out/seph23_gap.txt", seph)
    val _ = write ("out/automerge_gap.txt", automerge)
  in
    ()
  end

val _ = main ()
