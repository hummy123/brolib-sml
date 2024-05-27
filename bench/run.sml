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

  fun runTxnsTime () =
    let
      val startTime = Time.now ()
      val startTime = Time.toMilliseconds startTime

      val x = runTxns ()

      val endTime = Time.now ()
      val endTime = Time.toMilliseconds endTime

      val timeDiff = endTime - startTime
      val timeDiff = LargeInt.toString timeDiff
      val timeTook = String.concat [timeDiff, " ms taken for " ^ Txn.title ^ " txns\n"]
      val _ = (print timeTook)
    in
      x
    end

  fun write (fileName, buffer) =
    let
      val str = Txn.toString buffer
      val io = TextIO.openOut fileName
      val _ = TextIO.output (io, str)
      val _ = TextIO.closeOut io
    in
      ()
    end

  fun run () =
    let
      val buffer = runTxnsTime ()
      (* The write operation guarantees that MLton doesn't optimise away the
       * buffer's contents, because it has to write the contents to a file. *)
      val _ = write ("out/" ^ Txn.title ^ ".txt", buffer)
    in
      ()
    end
end
