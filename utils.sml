fun time_func title f =
  let
    val title = String.concat ["Starting " , title , "..."]
    val _ = (print title)
    val start_time = Time.now()
    val start_time = Time.toNanoseconds start_time
    val x = f()
    val end_time = Time.now()
    val end_time = Time.toNanoseconds end_time
    val time_diff = end_time - start_time 
    val time_diff = LargeInt.toString time_diff
    val time_took = String.concat ["took ", time_diff, " nanoseconds\n"]
    val _ = (print time_took)
  in
    x
  end

fun run_txns arr =
  Vector.foldl 
    (fn ((pos, del_num, ins_str), rope) =>
      let
        val rope = 
          if del_num > 0 
          then Rope.delete(pos, del_num, rope)
          else rope
        val str_size = String.size ins_str
        val rope = 
          if str_size > 0 
          then Rope.insert(pos, ins_str, rope)
          else rope
      in
        rope
      end) 
    Rope.empty arr

fun run_txns_time title arr = 
  let
    val f = (fn () => run_txns arr)
  in
    time_func title f
  end

fun run_to_string rope =
  Rope.to_string rope

fun run_to_string_time title rope =
  let
    val f = (fn () => run_to_string rope)
  in
    time_func title f
  end

fun run_txns_1000_times (counter, arr, total) =
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
      val start_time = Time.now()
      val start_time = Time.toNanoseconds start_time

      val _ = run_txns arr

      val end_time = Time.now()
      val end_time = Time.toNanoseconds end_time
      val time_diff = end_time - start_time 
      val counter = counter + 1
      val total = time_diff + total
    in
      run_txns_1000_times (counter, arr, total)
    end

fun write_file filename acc =
  let 
    val str = String.concatWith "," acc
    val fd = TextIO.openOut filename
    val _ = TextIO.output (fd, str) handle e => (TextIO.closeOut fd; raise e)
    val _ = TextIO.closeOut fd
  in
    ()
  end

val _ =
  let
    val start_time = LargeInt.fromInt 0
    val svelte = run_txns_1000_times (999 ,svelte_arr, start_time )

    val rust = run_txns_1000_times (999 ,rust_arr, start_time )

    val seph = run_txns_1000_times (999, seph_arr, start_time)

    val automerge = run_txns_1000_times (999, automerge_arr , start_time)
  in
    ()
  end
