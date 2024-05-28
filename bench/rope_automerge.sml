structure RopeAutomerge: TRANSACTION =
struct
  type t = TinyRope.t
  val empty = TinyRope.empty
  val insert = TinyRope.insert
  val delete = TinyRope.delete
  val toString = TinyRope.toString
  val txns = AutomergePaper.txns
end

structure Main = Run(RopeAutomerge)

val _ = Main.run ()
