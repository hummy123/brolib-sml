fun runTxns arr =
  Vector.foldl
    (fn ((pos, delNum, insStr), rope) =>
       let
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
  Vector.foldli (fn (idx, (pos, delNum, insStr), (rope, gapBuffer)) =>
    let
      val strSize = String.size insStr

      val rope = if strSize > 0 then TinyRope.insert (pos, insStr, rope) else
        rope
      val gapBuffer = if strSize > 0 then GapBuffer.insert (pos, insStr,
      gapBuffer) else gapBuffer

      val ropeString = TinyRope.toString rope
      val gapBufferString = GapBuffer.toString gapBuffer
    in
      if ropeString = gapBufferString then
        (rope, gapBuffer)
      else
        let
          val _ = print ("difference detected at txn number: " ^ (Int.toString idx) ^ "\n")
          val _ = print "rope string: \n"
          val _ = print (ropeString ^ "\n")
          val _ = print "gap string: \n"
          val _ = print (gapBufferString ^ "\n")
          val _ = raise Empty
        in
          (rope, gapBuffer)
        end
    end
  ) (TinyRope.empty, GapBuffer.empty) arr

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

fun loop () = loop()

fun main () =
  let
    (* Timing benchmarks. *)
    val _ = runTxnsTime SvelteComponent.txns
    val _ = runTxnsTime rust_arr
    val _ = runTxnsTime seph_arr
    val _ = runTxnsTime automerge_arr

    (* Tests for correctness; will fail if incorrect. *)
    val svelte = runTxns SvelteComponent.txns
    val rust = runTxns rust_arr
    val seph = runTxns seph_arr
    val automerge = runTxns automerge_arr

    (* Tests for insertion correctness (compare against rope). *)
    val _ = compareTxns SvelteComponent.txns
    val _ = compareTxns rust_arr
    val _ = compareTxns seph_arr
    val _ = compareTxns automerge_arr

    (* Tests for line metadata. *)
    (*
      val _ = Rope.verifyLines svelte
      val _ = Rope.verifyLines rust
      val _ = Rope.verifyLines seph
      val _ = Rope.verifyLines automerge
    *)

    val _ = write ("svelte_gap.txt", svelte)
    val _ = write ("rust23_gap.txt", rust)
    val _ = write ("seph23_gap.txt", seph)
    val _ = write ("automerge_gap.txt", automerge)
  in
    loop ()
  end

val _ = main ()
