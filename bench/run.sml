functor Run(Txn: TRANSACTION) =
struct
  local
    fun folder ((pos, delNum, insStr), buffer) =
      let
        val buffer =
          if delNum > 0 then Txn.delete (pos, delNum, buffer) else buffer
      in
        if String.size insStr > 0 then Txn.insert (pos, insStr, buffer)
        else buffer
      end
  in
    fun runTxns () =
      Vector.foldl folder Txn.empty Txn.txns
  end

  fun run () =
    let
      val buffer = runTxns ()
    in
      ()
    end
end
