structure LineGapRust: TRANSACTION =
struct
  type t = LineGap.t
  val empty = LineGap.empty
  val insert = LineGap.insert
  val delete = LineGap.delete
  val toString = LineGap.toString
  val txns = RustCode.txns
end

structure Main = Run(LineGapRust)

val _ = Main.run ()
