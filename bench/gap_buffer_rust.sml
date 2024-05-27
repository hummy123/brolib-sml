structure GapBufferRust: TRANSACTION =
struct
  type t = GapBuffer.t
  val empty = GapBuffer.empty
  val insert = GapBuffer.insert
  val delete = GapBuffer.delete
  val toString = GapBuffer.toString

  val title = "Rust"
  val txns = RustCode.txns
end

structure Main = Run(GapBufferRust)

val _ = Main.run ()
