structure GapBufferSeph: TRANSACTION =
struct
  type t = GapBuffer.t
  val empty = GapBuffer.empty
  val insert = GapBuffer.insert
  val delete = GapBuffer.delete
  val toString = GapBuffer.toString

  val title = "Seph"
  val txns = SephBlog.txns
end

structure Main = Run(GapBufferSeph)

val _ = Main.run ()
