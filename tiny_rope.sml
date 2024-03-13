signature TINY_ROPE =
sig
  type t
  val empty: t
  val fromString: string -> t
  val size: t -> int
  val insert: int * string * t -> t
  val append: string * t -> t
  val delete: int * int * t -> t
  val toString: t -> string
end

structure TinyRope :> TINY_ROPE =
struct
  datatype t =
    N0 of string
  | N1 of t
  | N2 of t * int * t
  | L2 of string * string
  | N3 of t * t * t

  exception AuxConstructor

  exception Substring of int

  fun foldr (f, state, rope) =
    case rope of
      N2 (l, _, r) =>
        let val state = foldr (f, state, r)
        in foldr (f, state, l)
        end
    | N1 t => foldr (f, state, t)
    | N0 s => f (state, s)
    | _ => raise AuxConstructor

  fun toString rope =
    let val strList = foldr ((fn (acc, str) => str :: acc), [], rope)
    in String.concat strList
    end

  datatype balance = AddedNode | DeletedNode | NoAction

  val targetLength = 1024
  val empty = N0 ""
  fun fromString string = N0 string

  fun isLessThanTarget (str1, str2) =
    String.size str1 + String.size str2 <= targetLength

  fun helpSize (acc, rope) =
    case rope of
      N0 s => acc + String.size s
    | N1 t => helpSize (acc, t)
    | N2 (_, lm, r) => helpSize (acc + lm, r)
    | _ => raise AuxConstructor

  fun size rope = helpSize (0, rope)

  fun insRoot rope =
    case rope of
      L2 (s1, s2) => N2 (N0 s1, String.size s1, N0 s2)
    | N3 (t1, t2, t3) =>
        let val left = N2 (t1, size t1, t2)
        in N2 (left, size left, N1 t3)
        end
    | t => t

  fun delRoot rope =
    case rope of
      N1 t => t
    | t => t

  fun insN1 rope =
    case rope of
      L2 (s1, s2) => N2 (N0 s1, String.size s1, N0 s2)
    | N3 (t1, t2, t3) =>
        let val left = N2 (t1, size t1, t2)
        in N2 (left, size left, N1 t3)
        end
    | t => N1 t

  fun insN2Left (left, right) =
    case (left, right) of
      (L2 (s1, s2), t3) => N3 (N0 s1, N0 s2, t3)
    | (N3 (t1, t2, t3), N1 t4) =>
        let
          val left = N2 (t1, size t1, t2)
          val right = N2 (t3, size t3, t4)
        in
          N2 (left, size left, right)
        end
    | (N3 (t1, t2, t3), t4) =>
        let val left = N2 (t1, size t1, t2)
        in N3 (left, N1 t3, t4)
        end
    | (l, r) => N2 (l, size l, r)

  fun delN2Left (left, right) =
    case (left, right) of
      (N1 t1, N1 t2) => N1 (N2 (t1, size t1, t2))
    | (N1 (N1 t1), N2 (N1 t2, _, (t3 as N2 _))) =>
        let
          val left = N2 (t1, size t1, t2)
          val inner = N2 (left, size left, t3)
        in
          N1 inner
        end
    | (N1 (N1 t1), N2 (N2 (t2, _, t3), _, N1 t4)) =>
        let
          val left = N2 (t1, size t1, t2)
          val right = N2 (t3, size t3, t4)
          val inner = N2 (left, size left, right)
        in
          N1 inner
        end
    | (N1 (t1 as N1 _), N2 ((t2 as N2 _), _, (t3 as N2 _))) =>
        let
          val left = N2 (t1, size t1, t2)
          val right = N1 t3
        in
          N2 (left, size left, right)
        end
    | (l, r) => N2 (l, size l, r)

  fun insN2Right (left, right) =
    case (left, right) of
      (t1, L2 (s1, s2)) => N3 (t1, N0 s1, N0 s2)
    | (N1 t1, N3 (t2, t3, t4)) =>
        let
          val left = N2 (t1, size t1, t2)
          val right = N2 (t3, size t3, t4)
        in
          N2 (left, size left, right)
        end
    | (t1, N3 (t2, t3, t4)) =>
        let val right = N2 (t3, size t3, t4)
        in N3 (t1, N1 t2, right)
        end
    | (l, r) => N2 (l, size l, r)

  fun delN2Right (left, right) =
    case (left, right) of
      (N2 (N1 t1, _, N2 (t2, _, t3)), N1 (N1 t4)) =>
        let
          val left = N2 (t1, size t1, t2)
          val right = N2 (t3, size t3, t4)
          val inner = N2 (left, size left, right)
        in
          N1 inner
        end
    | (N2 ((t1 as N2 _), lm, N1 t2), N1 (N1 t3)) =>
        let
          val right = N2 (t2, size t2, t3)
          val inner = N2 (t1, lm, right)
        in
          N1 inner
        end
    | (N2 ((t1 as N2 _), _, (t2 as N2 _)), N1 (t3 as N1 _)) =>
        let
          val left = N1 t1
          val right = N2 (t2, size t2, t3)
        in
          N2 (left, size left, right)
        end
    | (l, r) => N2 (l, size l, r)

  fun insLeaf (curIdx, newStr, rope, oldStr) =
    if curIdx <= 0 then
      if isLessThanTarget (oldStr, newStr) then (N0 (newStr ^ oldStr), NoAction)
      else (L2 (newStr, oldStr), AddedNode)
    else if curIdx >= String.size oldStr then
      if isLessThanTarget (oldStr, newStr) then (N0 (oldStr ^ newStr), NoAction)
      else (L2 (oldStr, newStr), AddedNode)
    else
      (* Need to split in middle of string. *)
      let
        val sub1 = String.substring (oldStr, 0, curIdx)
        val sub2Len = String.size oldStr - curIdx
        val sub2 = String.substring (oldStr, curIdx, sub2Len)
      in
        if
          isLessThanTarget (oldStr, newStr)
        then
          (N0 (sub1 ^ newStr ^ sub2), NoAction)
        else if
          curIdx + String.size newStr <= targetLength
        then
          (L2 (sub1 ^ newStr, sub2), AddedNode)
        else if
          ((String.size oldStr) - curIdx) + String.size newStr <= targetLength
        then
          (L2 (sub1, newStr ^ sub2), AddedNode)
        else
          (N3 (N0 sub1, N0 newStr, N0 sub2), AddedNode)
      end

  fun ins (curIdx, newStr, rope) =
    case rope of
      N2 (l, lm, r) =>
        if curIdx < lm then
          let
            val (l, action) = ins (curIdx, newStr, l)
          in
            (case action of
               NoAction =>
                 (case (l, r) of
                    (N0 s1, N0 s2) =>
                      if isLessThanTarget (s1, s2) then
                        (N0 (s1 ^ s2), DeletedNode)
                      else
                        (N2 (l, lm + String.size newStr, r), action)
                  | _ => (N2 (l, lm + String.size newStr, r), action))
             | AddedNode => (insN2Left (l, r), action)
             | DeletedNode => (delN2Left (l, r), action))
          end
        else
          let
            val (r, action) = ins (curIdx - lm, newStr, r)
          in
            (case action of
               NoAction =>
                 (case (l, r) of
                    (N0 s1, N0 s2) =>
                      if isLessThanTarget (s1, s2) then
                        (N0 (s1 ^ s2), DeletedNode)
                      else
                        (N2 (l, lm, r), action)
                  | _ => (N2 (l, lm, r), action))
             | AddedNode => (insN2Right (l, r), action)
             | DeletedNode => (delN2Right (l, r), action))
          end
    | N1 t =>
        let
          val (t, action) = ins (curIdx, newStr, t)
        in
          (case action of
             AddedNode => (insN1 t, action)
           | _ => (N1 t, action))
        end
    | N0 oldStr => insLeaf (curIdx, newStr, rope, oldStr)
    | _ => raise AuxConstructor

  fun insert (index, str, rope) =
    let
      val (rope, action) = ins (index, str, rope)
    in
      (case action of
         NoAction => rope
       | AddedNode => insRoot rope
       | DeletedNode => delRoot rope)
    end

  fun app (newStr, rope) =
    case rope of
      N2 (l, lm, r) =>
        let
          val (r, action) = app (newStr, r)
        in
          (case action of
             NoAction =>
               (case (l, r) of
                  (N0 s1, N0 s2) =>
                    if isLessThanTarget (s1, s2) then
                      (N0 (s1 ^ s2), DeletedNode)
                    else
                      (N2 (l, lm, r), action)
                | _ => (N2 (l, lm, r), action))
           | AddedNode => (insN2Right (l, r), action)
           | DeletedNode => (delN2Right (l, r), action))
        end
    | N1 t =>
        let
          val (t, action) = app (newStr, t)
        in
          (case action of
             AddedNode => (insN1 t, action)
           | _ => (N1 t, action))
        end
    | N0 oldStr =>
        if isLessThanTarget (oldStr, newStr) then
          (N0 (oldStr ^ newStr), NoAction)
        else
          (L2 (oldStr, newStr), AddedNode)
    | _ => raise AuxConstructor

  fun append (str, rope) =
    let
      val (rope, action) = app (str, rope)
    in
      (case action of
         NoAction => rope
       | AddedNode => insRoot rope
       | DeletedNode => delRoot rope)
    end

  fun delLeaf (startIdx, endIdx, str) =
    if startIdx <= 0 andalso endIdx >= String.size str then
      (empty, false)
    else if startIdx >= 0 andalso endIdx <= String.size str then
      let
        val sub1 = String.substring (str, 0, startIdx)
        val sub2 = String.substring (str, endIdx, (String.size str - endIdx))
      in
        if isLessThanTarget (sub1, sub2) then (N0 (sub1 ^ sub2), false)
        else (L2 (sub1, sub2), true)
      end
    else if startIdx >= 0 andalso endIdx >= String.size str then
      let
        val start = Int.toString startIdx
        val str = String.substring (str, 0, startIdx)
      in
        (N0 str, false)
      end
    else
      let val str = String.substring (str, endIdx, String.size str - endIdx)
      in (N0 str, false)
      end

  fun del (startIdx, endIdx, rope) =
    case rope of
      N2 (l, lm, r) =>
        if lm > startIdx andalso lm > endIdx then
          let
            val (l, didAdd) = del (startIdx, endIdx, l)
          in
            if didAdd then (insN2Left (l, r), didAdd)
            else (N2 (l, size l, r), didAdd)
          end
        else if lm < startIdx andalso lm < endIdx then
          let
            val (r, didAdd) = del (startIdx - lm, endIdx - lm, r)
          in
            if didAdd then (insN2Right (l, r), didAdd)
            else (N2 (l, lm, r), didAdd)
          end
        else
          let
            val (r, didAddR) = del (startIdx - lm, endIdx - lm, r)
            val (l, didaddL) = del (startIdx, endIdx, l)
          in
            if didaddL then (insN2Left (l, r), didaddL)
            else if didAddR then (insN2Right (l, r), didAddR)
            else (N2 (l, size l, r), false)
          end
    | N1 t =>
        let val (t, didAdd) = del (startIdx, endIdx, t)
        in if didAdd then (insN1 t, didAdd) else (N1 t, didAdd)
        end
    | N0 str => delLeaf (startIdx, endIdx, str)
    | _ => raise AuxConstructor

  fun delete (start, length, rope) =
    let val (rope, didAdd) = del (start, start + length, rope)
    in if didAdd then insRoot rope else delRoot rope
    end
end
