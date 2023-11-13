datatype rope = 
  N0 of string
  | N1 of rope
  | N2 of rope * int * int * rope
  | L2 of string * string
  | N3 of rope * rope * rope

val target_length = 1024
val empty = N0 ""
fun of_string string = N0 string

fun size rope = 
   case rope of
   N0 s => String.size s
   | N1 t => size t
   | N2(_, lm, rm, _) => lm + rm
   | N3(t1, t2, t3) => 
      let
        val t1_size = size t1
        val t2_size = size t2
        val t3_size = size t3
      in
        t1_size + t2_size + t3_size
      end
    | _ => raise Empty

fun root rope =
  case rope of
  L2(s1, s2) => N2(N0 s1, String.size s1, String.size s2, N0 s2)
  | N3(t1, t2, t3) =>
    let
      val t1_size = size t1
      val t2_size = size t2
      val left = N2(t1, t1_size, t2_size, t2)
      val left_size = t1_size + t2_size
      val t3_size = size t3
    in
      N2(left, left_size, t3_size, N1 t3)
    end
    | t => t

fun n1 rope = 
  case rope of
    L2(s1, s2) => 
      N2(N0 s1, String.size s1, String.size s2, N0 s2)
  | N3(t1, t2, t3) =>
      let
        val t1_size = size t1
        val t2_size = size t2
        val left = N2(t1, t1_size, t2_size, t2)
        val left_size = t1_size + t2_size
        val t3_size = size t3
      in
        N2(left, left_size, t3_size, t3)
      end
  | t => N1 t

fun ins_n2_left left right = 
  case (left, right) of
    (L2(s1, s2), t3) => N3(N0 s1, N0 s2, t3)
  | (N3(t1, t2, t3), N1 t4) =>
      let
        val t1_size = size t1
        val t2_size = size t2 
        val left = N2(t1, t1_size, t2_size, t2)
        val left_size = t1_size + t2_size
        val t3_size = size t3
        val t4_size = size t4
        val right = N2(t3, t3_size, t4_size, t4)
        val right_size = t3_size + t4_size
      in
        N2(left, left_size, right_size, right)
      end
  | (N3(t1, t2, t3), t4 as N2 _) =>
      let
        val left = N2(t1, size t1, size t2, t2)
      in
        N3(left, N1 t3, t4)
      end
  | (N3(t1, t2, t3), t4) =>
      let
        val t1_size = size t1
        val t2_size = size t2
        val left = N2(t1, t1_size, t2_size, t2)
        val t3_size = size t3
        val t4_size = size t4
        val right = N2(t3, t3_size, t4_size, t4)
        val left_size = t1_size + t2_size
        val right_size = t3_size + t4_size
      in
        N2(left, left_size, right_size, right)
      end
  | (l, r) =>
      N2(l, size l, size r, r)

fun ins_n2_right left right =
  case (left, right) of
    (t1, L2(s1, s2)) => N3(t1, N0 s1, N0 s2)
  | (N1 t1, N3(t2, t3, t4)) =>
      let
        val t1_size = size t1
        val t2_size = size t2
        val left = N2(t1, t1_size, t2_size, t2)
        val t3_size = size t3
        val t4_size = size t4
        val right = N2(t3, t3_size, t4_size, t4)
        val right_size = t3_size + t4_size
        val left_size = t1_size + t2_size
      in
        N2(left, left_size, right_size, right)
      end
  | (t1 as N2 _, N3(t2, t3, t4)) =>
      let
        val right = N2(t3, size t3, size t4, t4)
      in
        N3(t1, N1 t2, right)
      end
  | (l, r) =>
      N2(l, size l, size r, r)

fun ins cur_index string rope =
  case rope of
    N0 str =>
      let
        val old_str_size = String.size str 
        val ins_str_size = String.size string
        val total_str_size = old_str_size + ins_str_size
        val left_size = ins_str_size + cur_index
        val prefer_left = left_size <= target_length
        val right_size = ins_str_size - cur_index
        val prefer_right = right_size <= target_length
      in
        if cur_index <= 0 then
            if total_str_size <= target_length then
              N0(string ^ str)
            else L2(string, str)
        else if cur_index >= old_str_size then
            if total_str_size <= target_length then
              N0(str ^ string)
            else L2(str, string)
        else if total_str_size <= target_length then
            let
              val str1 = String.substring(str, 0, cur_index)
              val str3 = String.substring(str, cur_index, old_str_size - cur_index)
            in
              N0(String.concat [str1, string, str3])
            end
        else 
            let
              val str1 = String.substring(str, 0, cur_index)
              val str3 = String.substring(str, cur_index, old_str_size - cur_index)
            in
              if prefer_left then
                L2(str1 ^ string, str3)
              else if prefer_right then
                L2(str1, string ^ str3)
              else
                N3(N0 str1, N0 string, N0 str3)
            end
      end
    | N1 t =>
        let
          val t = ins cur_index string t
        in
          n1 t
        end
    | N2(l, lm, _, r) =>
        if cur_index < lm then
          let
            val l = ins cur_index string l
          in
            ins_n2_left l r
          end
        else
          let 
            val next_index = cur_index - lm
            val r = ins next_index string r
          in
            ins_n2_right l r
          end
    | _ => raise Empty

fun insert index string rope =
  let
    val rope = ins index string rope
  in
    root rope
  end

fun sub start_idx end_idx acc rope =
  case rope of
    N0 str =>
      let 
        val str_size = String.size str
        val before_start = start_idx <= 0
        val after_start = start_idx >= 0
        val after_end = end_idx >= str_size
        val before_end = end_idx <= str_size
      in
        if before_start andalso after_end then
          (str :: acc)
        else if after_start andalso before_start then
          let
            val len = end_idx - start_idx
            val str = String.substring(str, start_idx, len)
          in
            (str :: acc)
          end
        else if after_start andalso after_end then
          let
            val len = str_size - start_idx
            val str = String.substring(str, start_idx, len)
          in
            (str :: acc)
          end
        else
          let
            val str = String.substring(str, 0, end_idx)
          in
            (str::acc)
          end
      end
    | N1 t =>
        sub start_idx end_idx acc t
    | N2(l, lm, _, r) =>
        let
          val starts_before = lm > start_idx
          val ends_before = lm > end_idx
          val starts_after = lm < start_idx
          val ends_after = lm < end_idx
        in
          if starts_before andalso ends_before then
            sub start_idx end_idx acc l
          else if starts_after andalso ends_after then
            let
              val next_start = start_idx - lm
              val next_end = end_idx - lm
            in
              sub next_start next_end acc r
            end
          else
            let
              val next_start = start_idx - lm
              val next_end = end_idx - lm
              val sub_acc = sub next_start next_end acc r
            in
              sub start_idx end_idx sub_acc l
            end
        end
    | _ => raise Empty

fun substring start length rope =
  let
    val finish = start + length
    val lst = sub start finish [] rope
  in
    String.concat lst
  end

fun del start_idx end_idx rope =
  case rope of
    N0 str =>
        let
          val str_size = String.size str
          val before_start = start_idx <= 0
          val after_end = end_idx >= str_size
          val after_start = start_idx >= 0
          val before_end = end_idx <= str_size
        in
          if before_start andalso after_end then
            (N0 "", false)
          else if after_start andalso before_end then
            let
              val str1 = String.substring(str, 0, start_idx)
              val del_len = str_size - end_idx
              val str2 = String.substring(str, start_idx, del_len)
              val new_len = str_size - del_len
            in
              if new_len <= target_length then
                (N0(str1 ^ str2), false)
              else
                (L2(str1, str2), true)
            end
          else if after_start andalso after_end then
            let
              val str = String.substring(str, 0, start_idx)
            in
              (N0 str, false)
            end
          else
            let 
              val len = str_size - end_idx
              val str = String.substring(str, end_idx, len)
            in
              (N0 str, false)
            end
        end
    | N1 t =>
        let
          val (t, did_ins) = del start_idx end_idx t 
          val t = if did_ins then n1 t else t
        in
          (t, did_ins)
        end
    | N2(l, lm, rm, r) =>
        let
          val start_is_less = lm > start_idx
          val end_is_less = lm > end_idx
          val start_is_more = lm < start_idx
          val end_is_more = lm < end_idx
        in
          if start_is_less andalso end_is_less then
            let
              val (l, did_ins) = del start_idx end_idx l
            in
              case did_ins of
                false =>
                  (N2(l, size l, rm, r), false)
              | true =>
                  (ins_n2_left l r, true)
            end
          else if start_is_more andalso end_is_more then
            let
              val next_start = start_idx - lm
              val next_end = end_idx - lm
              val (r, did_ins) = del next_start next_end r
            in
              case did_ins of
                  false =>
                    (N2(l, lm, size r, r), false)
                | true =>
                    (ins_n2_right l r, true)
            end
          else
            let
              val (l, did_ins_l) = del start_idx end_idx l
              val r_start = start_idx - lm
              val r_end = end_idx - lm
              val (r, did_ins_r) = del r_start r_end r
            in
              if did_ins_l then
                (ins_n2_left l r, true)
              else if did_ins_r then
                (ins_n2_right l r, true)
              else 
                (N2(l, size l, size r, r), false)
            end
        end
    | _ => raise Empty

fun delete start length rope =
  let
    val finish = start + length
    val (t, _) = del start finish rope
  in
    t
  end
