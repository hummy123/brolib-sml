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
          then delete pos del_num rope 
          else rope
        val str_size = String.size ins_str
        val rope = 
          if str_size > 0 
          then insert pos ins_str rope 
          else rope
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

fun run_to_string rope =
  to_string rope

fun run_to_string_time title rope =
  let
    val f = (fn () => run_to_string rope)
  in
    time_func title f
  end

val _ =
  let
    val svelte = run_txns_time "svelte" svelte_arr
    val rust = run_txns_time "rust" rust_arr
    val seph = run_txns_time "seph" seph_arr
    val automerge = run_txns_time "automerge" automerge_arr

    val _ = run_to_string_time "svelte to_string" svelte
    val _ = run_to_string_time "rust to_string" rust
    val _ = run_to_string_time "seph to_string" seph
    val _ = run_to_string_time "automerge to_string" automerge
  in
    ()
  end
