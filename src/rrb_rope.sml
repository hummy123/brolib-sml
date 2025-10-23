structure RrbRope =
struct
  val bits: Word.word = 0w5
  val width: Word.word = 0w32
  val mask: Word.word = 0w31

  datatype tree = BRANCH of tree vector | LEAF of int vector

  type t = {root: tree, shift: word, count: int}

  val empty: t = {root = LEAF (Vector.fromList []), shift = 0w0, count = 0}

  fun tailoff count =
    if count < 32 then
      0w0
    else
      let
        val w = Word.fromInt (count - 1)
        val w = Word.>> (w, bits)
      in
        Word.<< (w, bits)
      end

  datatype append_result = UPDATE | APPEND

  fun helpAppend (item, tree) =
    case tree of
      BRANCH n =>
        let
          val lastNode = Vector.sub (n, Vector.length n - 1)
        in
          case helpAppend (item, lastNode) of
            (UPDATE, newLast, newDepth) =>
              let val n = Vector.update (n, Vector.length n - 1, newLast)
              in (UPDATE, BRANCH n, newDepth + 1)
              end
          | (APPEND, newNode, newDepth) =>
              if Vector.length n = 32 then
                let val hewNode = BRANCH (Vector.fromList [newNode])
                in (APPEND, newNode, newDepth + 1)
                end
              else
                let val n = Vector.concat [n, Vector.fromList [newNode]]
                in (UPDATE, BRANCH n, newDepth + 1)
                end
        end
    | LEAF items =>
        if Vector.length items = 32 then
          let val appendLeaf = LEAF (Vector.fromList [item])
          in (APPEND, appendLeaf, 0)
          end
        else
          let val newLeaf = Vector.concat [items, Vector.fromList [item]]
          in (UPDATE, LEAF newLeaf, 0)
          end

  fun append (item, {shift, root, count}: t) =
    case helpAppend (item, root) of
      (UPDATE, updatedTree, newDepth) =>
        { count = count + 1
        , root = updatedTree
        , shift = let val w = Word.fromInt newDepth in w * bits end
        }
    | (APPEND, newLast, newDepth) =>
        let
          val root = BRANCH (Vector.fromList [root, newLast])
          val w = Word.fromInt newDepth
          val shift = w * bits
        in
          {count = count + 1, root = root, shift = shift}
        end

  fun getLast tree =
    case tree of
      BRANCH n => getLast (Vector.sub (n, Vector.length n - 1))
    | LEAF i => Vector.sub (i, Vector.length i - 1)

  fun helpGet (key: Word.word, level, tree) =
    case tree of
      BRANCH nodes =>
        let
          val w = Word.>> (key, level)
          val w = Word.andb (w, mask)
          val node = Vector.sub (nodes, Word.toInt w)
        in
          helpGet (key, level - bits, node)
        end
    | LEAF items =>
        let val idx = Word.andb (key, mask)
        in Vector.sub (items, Word.toInt idx)
        end

  fun get (key, {shift, root, count}: t) =
    let val key = Word.fromInt key
    in if key >= tailoff count then getLast root else helpGet (key, shift, root)
    end

  fun splitKeepingLeft (idx, level, tree) =
    case tree of
      BRANCH nodes =>
        let
          val w = Word.>> (idx, level)
          val w = Word.andb (w, mask)
          val nodeIdx = Word.toInt w

          val node = Vector.sub (nodes, nodeIdx)
          val newNode = splitKeepingLeft (idx, level - bits, node)
          val newNode = Vector.fromList [newNode]
          val newNode = VectorSlice.full newNode

          val newNodes = VectorSlice.slice (nodes, 0, SOME nodeIdx)
          val newNodes = VectorSlice.concat [newNodes, newNode]
        in
          BRANCH newNodes
        end
    | LEAF items =>
        let
          val w = Word.andb (idx, mask)
          val idx = Word.toInt w
          val items = VectorSlice.slice (items, 0, SOME idx)
          val items = VectorSlice.vector items
        in
          LEAF items
        end
end
