structure LineGapAutomerge: TRANSACTION =
struct
  type t = LineGap.t
  val empty = LineGap.empty
  val insert = LineGap.insert
  val delete = LineGap.delete
  val toString = LineGap.toString
  val txns = AutomergePaper.txns
end

structure Main = Run(LineGapAutomerge)

val _ = Main.run ()
