structure RopeSeph: TRANSACTION =
struct
  type t = TinyRope.t
  val empty = TinyRope.empty
  val insert = TinyRope.insert
  val delete = TinyRope.delete
  val toString = TinyRope.toString
  val txns = SephBlog.txns
end

structure Main = Run(RopeSeph)

val _ = Main.run ()
