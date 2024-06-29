structure CompareToRope =
struct
  local
    fun folder ((pos, delNum, insStr), buffer, fIns, fDel) =
      let
        val buffer =
          if String.size insStr > 0 then 
            fIns (pos, insStr, buffer) else buffer
      in
        buffer
      end
  in
    fun runTxns () =
      Vector.foldl folder Txn.empty Txn.txns
  end
end
