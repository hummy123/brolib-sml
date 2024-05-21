fun timeFun (title, f) =
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
         val rope = if delNum > 0 then TinyRope.delete (pos, delNum, rope) else rope
         val strSize = String.size insStr
         val rope =
           if strSize > 0 then TinyRope.insert (pos, insStr, rope) else rope
       in
         rope
       end) TinyRope.empty arr

fun runTxnsTime title arr =
  let val f = (fn () => runTxns arr)
  in timeFun title f
  end

fun runToString rope = TinyRope.toString rope

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

fun timeFun (title, f) = 
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

fun appendMany (ctr, limit, rope) =
  if ctr = limit then rope
  else
    let
      val rope = TinyRope.append ("hello, world!", rope)
    in
      appendMany (ctr + 1, limit, rope)
    end

val closure = fn () =>
   (appendMany (0, 2410, TinyRope.empty))

fun closure2 rope = fn () =>
  TinyRope.toString rope

fun loop () = loop ()

fun main () =
  let
    val rope =
      timeFun("tiny_rope append : ", closure)
    val str = timeFun ("tiny_rope toString: ", closure2 rope)
    val str = String.concat str
    val io = TextIO.openOut "hello.txt"
    val _ = TextIO.output (io, str)
  in
     ()
  end

val _ = main ()
