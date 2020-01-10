package afcens.afccase
import scala.xml._

object ScenarioMap {
  private var positions: Map[String, Position] = _

  def init(): Unit = {
    val xmlFile = XML.loadFile("fields-symbols.svg")

    positions = (
      for {
        positions <- xmlFile \ "g" if positions \@ "id" == "Positions"
        posCircle <- positions \ "circle"
        posId = posCircle \@ "id"
        posX = posCircle \@ "cx"
        posY = posCircle \@ "cy"
      } yield posId -> Position(posX.toDouble, posY.toDouble)
      ).toMap
  }

  def apply(id: String): Position = positions("Pos-" + id)

  def main(args: Array[String]) {
    ScenarioMap.init()
    println(positions.keys)
  }

  /*
  Positions:
    Charge-[1..3]
    Rest-[1..8]
    Field-1-[1..12]
    Field-2-[1..10]
    Field-3-[1..6]
    Field-4-[1..6]
    Field-5-[1..10]
    Free-1-[1..15]
    Free-2-[1..9]
    Free-3-[1..15]
*/

  val chargeCount = 3
  val restCount = 8

  val fieldCount = 5
  val fieldSizes = Map(1 -> 12, 2 -> 10, 3 -> 6, 4 -> 6, 5 -> 10)

  val freeCount = 3
  val freeSizes = Map(1 -> 15, 2 -> 9, 3 -> 15)

  def chargeId(idx: Int) = {
    assert(idx >= 1 && idx <= chargeCount)
    s"Charge-${idx}"
  }

  def restId(idx: Int) = {
    assert(idx >= 1 && idx <= restCount)
    s"Rest-${idx}"
  }

  def fieldId(idx: Int, subIdx: Int) = {
    assert(idx >= 1 && idx <= fieldCount)
    val fieldSize = fieldSizes(idx)
    assert(subIdx >= 1 && subIdx <= fieldSize)
    s"Field-${idx}-${subIdx}"
  }

  def freeId(idx: Int, subIdx: Int) = {
    assert(idx >= 1 && idx <= freeCount)
    val freeSize = freeSizes(idx)
    assert(subIdx >= 1 && subIdx <= freeSize)
    s"Free-${idx}-${subIdx}"
  }
}