fun time_func title f =
  let
    val title = String.concat ["Starting " , title , "..."]
    val _ = (print title)
    val start_time = Time.now()
    val start_time = Time.toMilliseconds start_time
    val x = f()
    val end_time = Time.now()
    val end_time = Time.toMilliseconds end_time
    val time_diff = end_time - start_time 
    val time_diff = LargeInt.toString time_diff
    val time_took = String.concat ["took", time_diff, " ms\n"]
    val _ = (print time_took)
  in
    x
  end

fun run_txns arr =
  Vector.foldl 
    (fn ((pos, del_num, ins_str), rope) =>
      let
        val rope = if del_num > 0 then delete pos del_num rope else rope
        val str_size = String.size ins_str
        val rope = if str_size > 0 then insert pos ins_str rope else rope
      in
        rope
      end) 
    empty arr

fun run_txns_time title arr = 
  let
    val f = (fn () => run_txns arr)
  in
    time_func title f
  end

val _ =
  let
    val _ = run_txns_time "svelte" svelte_arr
    val _ = run_txns_time "rust" rust_arr
    val _ = run_txns_time "seph" seph_arr
    val _ = run_txns_time "automerge" automerge_arr
  in
    ()
  end
