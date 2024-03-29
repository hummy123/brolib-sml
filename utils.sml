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
         val rope = if delNum > 0 then Rope.delete (pos, delNum, rope) else rope
         val strSize = String.size insStr
         val rope =
           if strSize > 0 then Rope.insert (pos, insStr, rope) else rope
       in
         rope
       end) Rope.empty arr

fun runTxnsTime title arr =
  let val f = (fn () => runTxns arr)
  in timeFun title f
  end

fun runToString rope = Rope.toString rope

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

fun main () =
  let
    (* Timing benchmarks. *)
    val startTime = LargeInt.fromInt 0
    val _ = runTxns1000Times (999, svelte_arr, startTime)
    val _ = runTxns1000Times (999, rust_arr, startTime)
    val _ = runTxns1000Times (999, seph_arr, startTime)
    val _ = runTxns1000Times (999, automerge_arr, startTime)

    (* Tests that line metadata is correct; will fail if incorrect. *)
    val svelte = runTxns svelte_arr
    val _ = Rope.verifyLines svelte

    val rust = runTxns rust_arr
    val _ = Rope.verifyLines rust

    val seph = runTxns seph_arr
    val _ = Rope.verifyLines seph

    val automerge = runTxns automerge_arr
    val _ = Rope.verifyLines automerge
  in
    ()
  end

val _ = main ()
