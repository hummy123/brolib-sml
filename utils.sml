fun timeFun title f =
  let
    val title = String.concat ["Starting ", title, "..."]
    val _ = (print title)
    val startTime = Time.now ()
    val startTime = Time.toNanoseconds startTime
    val x = f ()
    val endTime = Time.now ()
    val endTime = Time.toNanoseconds endTime
    val timeDiff = endTime - startTime
    val timeDiff = LargeInt.toString timeDiff
    val timeTook = String.concat ["took ", timeDiff, " nanoseconds\n"]
    val _ = (print timeTook)
  in
    x
  end

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

fun runTxnsTime title arr =
  let val f = (fn () => runTxns arr)
  in timeFun title f
  end

fun runToString rope = GapBuffer.toString rope

fun runToStringTime title rope =
  let val f = (fn () => runToString rope)
  in timeFun title f
  end

fun runTxns1000Times (counter, arr, total) =
  if counter = 1000 then
    let
      val divisor = Int.toLarge 1000
      val total = total div divisor
      val str = LargeInt.toString total
    in
      print (str ^ "\n")
    end
  else
    let
      val startTime = Time.now ()
      val startTime = Time.toNanoseconds startTime

      val _ = runTxns arr

      val endTime = Time.now ()
      val endTime = Time.toNanoseconds endTime
      val timeDiff = endTime - startTime
      val counter = counter + 1
      val total = timeDiff + total
    in
      runTxns1000Times (counter, arr, total)
    end

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
    val _ = compareTxns SvelteComponent.txns

    val startTime = LargeInt.fromInt 0
    val _ = runTxns1000Times (999, SvelteComponent.txns, startTime)
    val _ = runTxns1000Times (999, rust_arr, startTime)
    val _ = runTxns1000Times (999, seph_arr, startTime)
    val _ = runTxns1000Times (999, automerge_arr, startTime)

    (* Tests that line metadata is correct; will fail if incorrect. *)
    val svelte = runTxns SvelteComponent.txns
    val rust = runTxns rust_arr
    val seph = runTxns seph_arr
    val automerge = runTxns automerge_arr

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
