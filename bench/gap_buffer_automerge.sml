structure GapBufferAutomerge: TRANSACTION =
struct
  type t = GapBuffer.t
  val empty = GapBuffer.empty
  val insert = GapBuffer.insert
  val delete = GapBuffer.delete
  val toString = GapBuffer.toString

  val title = "Automerge"
  val txns = AutomergePaper.txns
end

structure Main = Run(GapBufferAutomerge)

val _ = Main.run ()
