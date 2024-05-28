structure RopeSvelte: TRANSACTION =
struct
  type t = TinyRope.t
  val empty = TinyRope.empty
  val insert = TinyRope.insert
  val delete = TinyRope.delete
  val toString = TinyRope.toString
  val txns = SvelteComponent.txns
end

structure Main = Run(RopeSvelte)

val _ = Main.run ()
