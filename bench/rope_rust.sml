structure RopeRust: TRANSACTION =
struct
  type t = TinyRope.t
  val empty = TinyRope.empty
  val insert = TinyRope.insert
  val delete = TinyRope.delete
  val toString = TinyRope.toString
  val txns = RustCode.txns
end

structure Main = Run(RopeRust)

val _ = Main.run ()
