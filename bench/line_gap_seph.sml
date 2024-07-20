structure LineGapSeph: TRANSACTION =
struct
  type t = LineGap.t
  val empty = LineGap.empty
  val insert = LineGap.insert
  val delete = LineGap.delete
  val toString = LineGap.toString
  val txns = SephBlog.txns
end

structure Main = Run(LineGapSeph)

val _ = Main.run ()
