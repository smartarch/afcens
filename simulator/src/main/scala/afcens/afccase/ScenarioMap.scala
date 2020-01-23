package afcens.afccase

import scala.collection.immutable
import scala.util.Random
import scala.xml._

object PositionType extends Enumeration {
  type PositionType = Value
  val CHARGE, REST, FIELD, FREE = Value
}

import afcens.afccase.PositionType._

abstract class PositionId {
  def position: Position = ScenarioMap(toString)
}

object PositionId {
  def getClosestTo[T >: Null <: PositionId](position: Position, positionIds: Iterable[T]): T = {
    var closestId: T = null
    var closestDistance: Double = 0

    for (positionId <- positionIds) {
      val distance = positionId.position.distance(position)
      if (closestId == null || distance < closestDistance) {
        closestId = positionId
        closestDistance = distance
      }
    }

    closestId
  }
}

case class ChargerId(idx: Int) extends PositionId {
  assert(idx >= 0 && idx < ScenarioMap.chargerCount)

  override def toString: String = s"Charge-${idx + 1}"
}

case class RestId(idx: Int) extends PositionId {
  assert(idx >= 0 && idx < ScenarioMap.restCount)

  override def toString: String = s"Rest-${idx + 1}"
}

case class Area(left: Double, top: Double, right: Double, bottom: Double) {
  def contains(pos: Position): Boolean = pos != null && pos.x >= left && pos.y >= top && pos.x <= right && pos.y <= bottom
}

object FieldIdHelper {
  private def _computeFieldArea(idx: Int): Area = {
    val allPositions = (for (subIdx <- 0 until ScenarioMap.fieldSizes(idx)) yield FieldId(idx, subIdx).position).toList

    Area(
      allPositions.map(_.x).reduce((a, b) => Math.min(a, b)),
      allPositions.map(_.y).reduce((a, b) => Math.min(a, b)),
      allPositions.map(_.x).reduce((a, b) => Math.max(a, b)),
      allPositions.map(_.y).reduce((a, b) => Math.max(a, b))
    )
  }

  val _fieldAreas = Map.empty[Int, Area] ++ (for (idx <- 0 until ScenarioMap.fieldCount) yield idx -> _computeFieldArea(idx))

  def center(fieldIdx: Int) = centers(fieldIdx, 1).head

  def centers(fieldIdx: Int, horizClusterCount: Int) = {
    val fa = _fieldAreas(fieldIdx)
    val width = (fa.right - fa.left) / horizClusterCount
    val yc = (fa.bottom + fa.top) / 2
    val startXc = fa.left + width / 2

    (for (idx <- 0 until horizClusterCount) yield Position(startXc + width * idx, yc))
  }

  def area(fieldIdx: Int) = _fieldAreas(fieldIdx)

  def protectingDroneCountRequired(fieldIdx: Int) = {
    val fa = _fieldAreas(fieldIdx)
    val width = fa.right - fa.left
    Math.ceil(width / (Flock.disturbRadius * 2)).toInt
  }
}

case class FieldId(idx: Int, subIdx: Int) extends PositionId {
  {
    assert(idx >= 0 && idx < ScenarioMap.fieldCount)
    val fieldSize = ScenarioMap.fieldSizes(idx)
    assert(subIdx >= 0 && subIdx < fieldSize)
  }

  override def toString: String = s"Field-${idx + 1}-${subIdx + 1}"

  def sameFieldIds: Seq[FieldId] = for {
    otherSubIdx <- 0 until ScenarioMap.fieldSizes(idx)
  } yield FieldId(idx, otherSubIdx)
}

case class FreeId(idx: Int, subIdx: Int) extends PositionId {
  assert(idx >= 0 && idx < ScenarioMap.freeCount)
  val freeSize = ScenarioMap.freeSizes(idx)
  assert(subIdx >= 0 && subIdx < freeSize)

  override def toString: String = s"Free-${idx + 1}-${subIdx + 1}"
}

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

  val chargerCount = 3
  val restCount = 8

  val fieldCount = 5
  val fieldSizes = Map(0 -> 12, 1 -> 10, 2 -> 6, 3 -> 6, 4 -> 10)

  val freeCount = 3
  val freeSizes = Map(0 -> 15, 1 -> 9, 2 -> 15)

  val allChargerIds = for {
    idx <- 0 until chargerCount
  } yield ChargerId(idx)

  val allFieldIds = for {
    idx <- 0 until fieldCount
    subIdx <- 0 until fieldSizes(idx)
  } yield FieldId(idx, subIdx)

  def randomFieldId(implicit rand: Random): FieldId = allFieldIds(rand.nextInt(allFieldIds.length))

  val allFreeIds = for {
    idx <- 0 until freeCount
    subIdx <- 0 until freeSizes(idx)
  } yield FreeId(idx, subIdx)

  def randomFreeId(implicit rand: Random): FreeId = allFreeIds(rand.nextInt(allFreeIds.length))

}