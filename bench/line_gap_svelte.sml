structure LineGapSvelete: TRANSACTION =
struct
  type t = LineGap.t
  val empty = LineGap.empty
  val insert = LineGap.insert
  val delete = LineGap.delete
  val toString = LineGap.toString
  val txns = SvelteComponent.txns
end

structure Main = Run(LineGapSvelete)

val _ = Main.run ()
