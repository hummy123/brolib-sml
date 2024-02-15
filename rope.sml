signature ROPE = sig
  type t
  val empty : t
  val of_string : string -> t
  val size : t -> int
  val insert : int * string * t -> t
  val delete : int * int * t -> t
  val to_string : t -> string
end

structure Rope :> ROPE = struct
  datatype t  
    = N0 of string
    | N1 of t
    | N2 of t * int * t
    | L2 of string * string
    | N3 of t * t * t

  exception AuxConstructor

  exception Substring of int

  fun fold_right (f, state, rope) =
    case rope of
      N2(l, _, r) =>
        let
          val state = fold_right(f, state, r)
        in
          fold_right(f, state, l)
        end
    | N1 t =>
        fold_right(f, state, t)
    | N0 s =>
        f (state, s)
    | _ =>
        raise AuxConstructor

  fun to_string rope =
    let
      val str_list = fold_right((fn (acc, str) => str::acc), [], rope)
    in
      String.concat str_list
    end

  datatype balance  
    = AddedNode
    | DeletedNode
    | NoAction

  val target_length = 1024
  val empty = N0 ""
  fun of_string string = N0 string

  fun is_less_than_target(str1, str2) =
    String.size str1 + String.size str2 <= target_length

  fun help_size(acc, rope) =
    case rope of
      N0 s => 
        acc + String.size s
    | N1 t => 
        help_size(acc, t)
    | N2(_, lm, r) =>
        help_size(acc + lm, r)
    | _ => raise AuxConstructor

  fun size rope = help_size(0, rope)

  fun ins_root rope =
    case rope of
      L2(s1, s2) => 
        N2(N0 s1, String.size s1, N0 s2)
    | N3(t1, t2, t3) =>
        let
          val left = N2(t1, size t1, t2)
        in
          N2(left, size left, N1 t3)
        end
    | t =>
        t

  fun del_root rope =
    case rope of
      N1 t => t
    | t => t

  fun ins_n1 rope =
    case rope of
        L2 (s1, s2) =>
          N2(N0 s1, String.size s1, N0 s2)
      | N3(t1, t2, t3) =>
          let 
            val left = N2(t1, size t1, t2)
          in
            N2(left, size left, N1 t3)
          end
      | t =>
          N1 t

  fun ins_n2_left (left, right) =
    case (left, right) of
      (L2(s1, s2), t3) =>
        N3(N0 s1, N0 s2, t3)
    | (N3(t1, t2, t3), N1 t4) =>
        let
          val left = N2(t1, size t1, t2)
          val right = N2(t3, size t3, t4)
        in
          N2(left, size left, right)
        end
    | (N3(t1, t2, t3), t4) =>
        let 
          val left = N2(t1, size t1, t2)
        in
          N3(left, N1 t3, t4)
        end
    | (l, r) =>
        N2(l, size l, r)

  fun del_n2_left(left, right) = 
    case (left, right) of
      (N1 t1, N1 t2) =>
        N1(N2(t1, size t1, t2))
    | (N1 (N1 t1), N2(N1 t2, _, (t3 as N2 _))) =>
        let
          val left = N2(t1, size t1, t2)
          val inner = N2(left, size left, t3)
        in
          N1 (inner)
        end
    | (N1 (N1 t1), N2(N2(t2, _, t3), _, N1 t4)) =>
        let
          val left = N2(t1, size t1, t2)
          val right = N2(t3, size t3, t4)
          val inner = N2(left, size left, right)
        in
          N1 inner
        end
    | (N1 (t1 as N1 _), N2((t2 as N2 _), _, (t3 as N2 _))) =>
        let
          val left = N2(t1, size t1, t2)
          val right = N1 t3
        in
          N2(left, size left, right)
        end
    | (l, r) =>
        N2(l, size l, r)

  fun ins_n2_right (left, right) =
    case (left, right) of
      (t1, L2(s1, s2)) =>
        N3(t1, N0 s1, N0 s2)
    | (N1 t1, N3(t2, t3, t4)) =>
        let
          val left = N2(t1, size t1, t2)
          val right = N2(t3, size t3, t4)
        in
          N2(left, size left, right)
        end
    | (t1, N3(t2, t3, t4)) =>
        let
          val right = N2(t3, size t3, t4)
        in
          N3(t1, N1 t2, right)
        end
    | (l, r) =>
        N2(l, size l, r)

  fun del_n2_right(left, right) =
    case (left, right) of
      (N2(N1 t1, _, N2(t2, _, t3)), N1 (N1 t4)) =>
        let
          val left = N2(t1, size t1, t2)
          val right = N2(t3, size t3, t4)
          val inner = N2(left, size left, right)
        in
          N1 inner
        end
    | (N2((t1 as N2 _), lm, N1 t2), N1 (N1 t3)) =>
        let
          val right = N2(t2, size t2, t3)
          val inner = N2(t1, lm, right)
        in
          N1 inner
        end
    | (N2( (t1 as N2 _), _, (t2 as N2 _)), N1 (t3 as N1 _)) =>
        let
          val left = N1 t1
          val right = N2(t2, size t2, t3)
        in
          N2(left, size left, right)
        end
    | (l, r) =>
        N2(l, size l, r)

    fun ins_leaf(cur_index, new_str, rope, old_str) =
      if cur_index <= 0 then
        if is_less_than_target(old_str, new_str) then
          (N0(new_str ^ old_str), NoAction)
        else
          (L2(new_str, old_str), AddedNode)
      else if cur_index >= String.size old_str then
        if is_less_than_target(old_str, new_str) then
          (N0(old_str ^ new_str), NoAction)
        else
          (L2(old_str, new_str), AddedNode)
      else
        (* Need to split in middle of string. *)
        let
          val sub1 = String.substring(old_str, 0, cur_index)
          val sub2_len = String.size old_str - cur_index
          val sub2 = String.substring(old_str, cur_index, sub2_len)
        in
          if is_less_than_target(old_str, new_str) then
            (N0(sub1 ^ new_str ^ sub2), NoAction)
          else if cur_index + String.size new_str <= target_length then
            (L2(sub1 ^ new_str, sub2), AddedNode)
          else if ((String.size old_str) - cur_index) + String.size new_str <= target_length then
            (L2(sub1, new_str ^ sub2), AddedNode)
          else
            (N3(N0 sub1, N0 new_str, N0 sub2), AddedNode)
        end

  fun ins(cur_index, new_str, rope) =
    case rope of
      N2(l, lm, r) =>
        if cur_index < lm then
          let 
            val (l, action) = ins(cur_index, new_str, l)
          in
            (case action of
              NoAction => 
                (case (l, r) of
                  (N0 s1, N0 s2) =>
                    if is_less_than_target(s1, s2) then
                      (N0 (s1 ^ s2), DeletedNode)
                    else
                      (N2(l, lm + String.size new_str, r), action)
                | _ =>
                  (N2(l, lm + String.size new_str, r), action))
            | AddedNode =>
                (ins_n2_left(l, r), action)
            | DeletedNode =>
                (del_n2_left(l, r), action))
          end
        else
          let
            val (r, action) = ins(cur_index - lm, new_str, r)
          in
            (case action of
              NoAction =>
                (case (l, r) of
                  (N0 s1, N0 s2) =>
                    if is_less_than_target(s1, s2) then
                      (N0 (s1 ^ s2), DeletedNode)
                    else
                      (N2(l, lm, r), action)
                  | _ =>
                      (N2(l, lm, r), action))
              | AddedNode =>
                  (ins_n2_right(l, r), action)
              | DeletedNode =>
                  (del_n2_right(l, r), action))
          end
    | N1 t =>
        let
          val (t, action) = ins(cur_index, new_str, t)
        in
          (case action of
            AddedNode =>
              (ins_n1 t, action)
          | _ =>
              (N1 t, action))
        end
    | N0 old_str => 
        ins_leaf(cur_index, new_str, rope, old_str)
    | _ =>
        raise AuxConstructor
 
  fun insert (index, str, rope) =
    let
      val (rope, action) = ins(index, str, rope)
    in
      (case action of
        NoAction =>
          rope
      | AddedNode =>
          ins_root rope
      | DeletedNode =>
          del_root rope)
    end

  fun del_leaf(start_idx, end_idx, str) =
    if start_idx <= 0 andalso end_idx >= String.size str then
      (empty, false)
    else if start_idx >= 0 andalso end_idx <= String.size str then
      let
        val sub1 = String.substring(str, 0, start_idx)
        val sub2 = String.substring(str, end_idx, (String.size str - end_idx))
      in
        if is_less_than_target(sub1, sub2) then
          (N0 (sub1 ^ sub2), false)
        else
          (L2(sub1, sub2), true)
      end
    else if start_idx >= 0 andalso end_idx >= String.size str then
      let
        val start = Int.toString start_idx
        val str = String.substring(str, 0, start_idx)
      in
        (N0 str, false)
      end
    else
      let
        val str = String.substring(str, end_idx, String.size str - end_idx)
      in
        (N0 str, false)
      end

  fun del (start_idx, end_idx, rope) =
    case rope of
      N2(l, lm, r) =>
        if lm > start_idx andalso lm > end_idx then
          let
            val (l, did_add) = del(start_idx, end_idx, l)
          in
            if did_add then 
              (ins_n2_left(l, r), did_add)
            else
              (N2(l, size l, r), did_add)
          end
        else if lm < start_idx andalso lm < end_idx then
          let
            val (r, did_add) = del(start_idx - lm, end_idx - lm, r)
          in
            if did_add then
              (ins_n2_right(l, r), did_add)
            else
              (N2(l, lm, r), did_add)
          end
        else
          let
            val (r, did_add_r) = del(start_idx - lm, end_idx - lm, r)
            val (l, did_add_l) = del(start_idx, end_idx, l)
          in
            if did_add_l then
              (ins_n2_left(l, r), did_add_l)
            else if did_add_r then
              (ins_n2_right(l, r), did_add_r)
            else
              (N2(l, size l, r), false)
          end
    | N1 t =>
        let
          val (t, did_add) = del(start_idx, end_idx, t)
        in
          if did_add then
            (ins_n1 t, did_add)
          else
            (N1 t, did_add)
        end
    | N0 str =>
        del_leaf(start_idx, end_idx, str)
    | _ =>
        raise AuxConstructor

  fun delete(start, length, rope) =
    let
      val (rope, did_add) = del(start, start + length, rope)
    in
      if did_add then
        ins_root rope
      else
        del_root rope
    end
end
