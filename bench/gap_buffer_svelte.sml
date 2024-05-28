structure GapBufferSvelete: TRANSACTION =
struct
  type t = GapBuffer.t
  val empty = GapBuffer.empty
  val insert = GapBuffer.insert
  val delete = GapBuffer.delete
  val toString = GapBuffer.toString
  val txns = SvelteComponent.txns
end

structure Main = Run(GapBufferSvelete)

val _ = Main.run ()
