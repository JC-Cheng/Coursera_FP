import streams._

object lv0 extends GameDef with StringParserTerrain with Solver {
  val level =
    """ooo-------
      |oSoooo----
      |----o-----
      |----o-----
      |----oooT--""".stripMargin
}

object lv1 extends GameDef with StringParserTerrain with Solver {
  val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin
}

object lvK extends GameDef with Solver with InfiniteTerrain {
  val startPos = Pos(5, 3)
  val goal = Pos(10, 2)
}

val lv = lvK

val L = lv.Left
val R = lv.Right
val U = lv.Up
val D = lv.Down

val b1 = lv.Block(lv.Pos(1, 1), lv.Pos(1, 1))
val b2 = lv.Block(lv.Pos(1, 2), lv.Pos(1, 3))
val b3 = lv.Block(lv.Pos(2, 1), lv.Pos(3, 1))

lv.neighborsWithHistory(b1, List(L, U)).toList
lv.newNeighborsOnly(Set((b2, List(R, L, U)), (b3, List(D, L, U))).toStream, Set(b2, b1)).toList

lv.solution


